#' @title
#' Perform gene set enrichment analysis with GSVA.
#'
#' @description
#' This function calculates enrichment scores, p- and q-value statistics for
#' provided gene sets for specified groups of cells in given Seurat object using
#' gene set variation analysis (GSVA). Calculation of p- and q-values for gene
#' sets is performed as done in "Evaluation of methods to assign cell type
#' labels to cell clusters from single-cell RNA-sequencing data", Diaz-Mejia et
#' al., F1000Research (2019).
#'
#' @param object Seurat object.
#' @param assay Assay to pull counts from; defaults to 'RNA'. Only relevant in
#' Seurat v3.0 or higher since the concept of assays wasn't implemented before.
#' @param GMT_file Path to GMT file containing the gene sets to be tested.
#' The Broad Institute provides many gene sets which can be downloaded:
#' http://software.broadinstitute.org/gsea/msigdb/index.jsp
#' @param groups Grouping variables (columns) in object@meta.data for which
#' gene set enrichment analysis should be performed
#' @param name Name of list that should be used to store the results in
#' object@misc$enriched_pathways$<name>; defaults to 'cerebro_GSVA'.
#' @param thresh_p_val Threshold for p-value, defaults to 0.05.
#' @param thresh_q_val Threshold for q-value, defaults to 0.1.
#' @param ... Further parameters can be passed to control GSVA::gsva().
#'
#' @return
#' Seurat object with GSVA results for the specified grouping variables
#' stored in object@misc$enriched_pathways$<name>
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.3/pbmc_seurat.rds",
#'   package = "cerebroApp"))
#' example_gene_set <- system.file("extdata/example_gene_set.gmt",
#'   package = "cerebroApp")
#' pbmc <- performGeneSetEnrichmentAnalysis(
#'   object = pbmc,
#'   GMT_file = example_gene_set,
#'   groups = c('sample','seurat_clusters'),
#'   thresh_p_val = 0.05,
#'   thresh_q_val = 0.1
#' )
#'
#' @import dplyr
#' @importFrom GSVA gsva
#' @importFrom Matrix colMeans colSums rowSums t
#' @importFrom qvalue qvalue
#' @importFrom rlang .data
#' @importFrom tibble tibble
#'
#' @export
#'
performGeneSetEnrichmentAnalysis <- function(
  object,
  assay = 'RNA',
  GMT_file,
  groups = NULL,
  name = 'cerebro_GSVA',
  thresh_p_val = 0.05,
  thresh_q_val = 0.1,
  ...
) {

  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  ## check if Seurat is installed
  if ( !requireNamespace("Seurat", quietly = TRUE) ) {
    stop(
      "The 'Seurat' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## check that Seurat package is at least v3.0
  if ( utils::packageVersion('Seurat') < 3 ) {
    stop(
      paste0(
        "The installed Seurat package is of version `", utils::packageVersion('Seurat'),
        "`, but at least v3.0 is required."
      ),
      call. = FALSE
    )
  }

  ## check if provided object is of class "Seurat"
  if ( class(object) != "Seurat" ) {
    stop(
      paste0(
        "Provided object is of class `", class(object), "` but must be of class 'Seurat'."
      ),
      call. = FALSE
    )
  }

  ## check version of Seurat object and stop if it is lower than 3
  if ( object@version < 3 ) {
    stop(
      paste0(
        "Provided Seurat object has version `", object@version, "` but must be at least 3.0."
      ),
      call. = FALSE
    )
  }

  ## check if provided assay exists
  if ( assay %in% names(object@assays) == FALSE ) {
    stop(
      paste0(
        'Specified assay slot `', assay, '` could not be found in provided Seurat object.'
      ),
      call. = FALSE
    )
  }

  ## check if `data` matrix exist in provided assay
  if ( is.null(object@assays[[assay]]@data) ) {
    stop(
      paste0(
        '`data` matrix could not be found in `', assay, '` assay slot of the provided Seurat object.'
      ),
      call. = FALSE
    )
  }

  ## check if specified GMT files exists
  if ( !file.exists(GMT_file) ) {
    stop("Specified GMT file with gene sets cannot be found.", call. = FALSE)
  }

  ## check if provided groups are present in meta data
  if ( any(which(groups %in% colnames(object@meta.data) == FALSE)) ) {
    missing_groups <- groups[which(groups %in% colnames(object@meta.data) == FALSE)]
    stop(
      paste0(
        "Group(s) `", paste0(missing_groups, collapse = '`, `'), "` were not ",
        "found in meta data of provided Seurat object. Only grouping variables ",
        "that are present in the meta data can be used."
      ),
      call. = FALSE
    )
  }

  ## check if specified p-value thresholds are between 0 and 1
  if ( thresh_p_val < 0 | thresh_p_val > 1 ) {
    stop(
      "Specified threshold for p-value must be between 0 and 1.",
      call. = FALSE
    )
  }

  ## check if specified q-value thresholds are between 0 and 1
  if ( thresh_q_val < 0 | thresh_q_val > 1 ) {
    stop(
      "Specified threshold for q-value must be between 0 and 1.",
      call. = FALSE
    )
  }

  ##---------------------------------------------------------------------------#
  ## preparation
  ##---------------------------------------------------------------------------#

  ## load gene sets from GMT file
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Loading gene sets...'
    )
  )
  gene_sets <- .read_GMT_file(GMT_file)

  ## get names of gene sets
  names(gene_sets$genesets) <- gene_sets$geneset.names

  ## make tibble that contains name and description for each set
  gene_sets_tibble <- tibble::tibble(
      name = gene_sets$geneset.names,
      description = gene_sets$geneset.description,
      length = NA,
      genes = NA
    )

  ## add number of genes and collapsed gene list to tibble of gene sets
  for ( i in seq_along(gene_sets$genesets) ) {
    gene_sets_tibble$length[i] <- gene_sets$genesets[[i]] %>% length()
    gene_sets_tibble$genes[i] <- gene_sets$genesets[[i]] %>%
      unlist() %>%
      paste(.data, collapse = ',')
  }

  ## log message
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Loaded ',
      length(gene_sets$genesets), ' gene sets from GMT file.'
    )
  )

  ## check which genes are not expressed in any cell
  expressed_genes <- Matrix::rowSums(object@assays[[assay]]@data)
  expressed_genes <- which(expressed_genes != 0)

  ## extract transcript counts for expressed genes from Seurat object
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Extracting transcript counts ',
      'from `data` slot of `', assay, '` assay...'
    )
  )
  matrix_full <- object@assays[[assay]]@data[expressed_genes,]

  ##---------------------------------------------------------------------------#
  ## perform gene set enrichment analysis for each group
  ##---------------------------------------------------------------------------#

  ## create slot for results
  object@misc$enriched_pathways[[ name ]] <- list()

  ##
  for ( i in seq_along(groups) ) {

    ## get current group
    current_group <- groups[i]

    ## collect group levels
    ## ... column contains factors
    if ( is.factor(object@meta.data[[ current_group ]]) ) {

      ## get factor levels
      group_levels <- levels(object@meta.data[[ current_group ]])

    ## ... column contains characters
    } else if ( is.character(object@meta.data[[ current_group ]]) ) {

      ## get unique entries in column
      group_levels <- unique(object@meta.data[[ current_group ]])

      ## check for NA values
      ## ... if at least 1 group level is NA
      if ( any(is.na(group_levels)) ) {

        ## get number of cells with NA as group assignment
        number_of_cells_without_group_assignment <- object@meta.data[[ current_group ]] %>%
          is.na() %>%
          which(. == TRUE) %>%
          length()

        ## remove NA entries from group levels
        group_levels <- stats::na.omit(group_levels)

        ## issue warning to user
        warning(
          paste0(
            'Found ', number_of_cells_without_group_assignment, ' cell(s) ',
            'without group assignment (NA) for `', current_group,
            '`. These cells will be ignored during the analysis.'
          ),
          call. = FALSE
        )
      }
    }

    ## log message
    message(
      glue::glue(
        '[', format(Sys.time(), '%H:%M:%S'), '] Performing analysis for ',
        '{length(group_levels)} subgroups of group `{current_group}`...'
      )
    )

    ## check number of group levels
    ## ... if only 1 group level is present, show warning and move to next
    ##     grouping variable
    if ( length(group_levels) == 1 ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] Only one group level found ',
          'for group `', current_group, '`. Will skip this group and proceed ',
          'to next.'
        ),
        call. = FALSE
      )
      results <- NULL

    ## ... more than 1 group level is available
    } else if ( length(group_levels) > 1 ) {

      ## get results
      matrix_mean_by_group <- future.apply::future_sapply(
        group_levels, USE.NAMES = TRUE, simplify = TRUE,
        future.globals = FALSE, function(x)
      {

        ## get indices of cells in this group level
        cells_in_this_group <- which(object@meta.data[[ current_group ]] == x)

        ## check how many cells are in the group level
        ## ... only 1 cell
        if ( length(cells_in_this_group) == 1 ) {

          ## return expression values from the single cell
          matrix_full[,cells_in_this_group]

        ## ... at least 2 cells
        } else {

          ## calculate mean expression for each gene across cells
          Matrix::rowMeans(matrix_full[,cells_in_this_group])
        }
      })

      ## get enrichment score for each gene set in every cell group
      enrichment_scores <- GSVA::gsva(
          expr = matrix_mean_by_group,
          gset.idx.list = gene_sets$genesets,
          ...
        )

      ## log message
      # message(
      #   paste0(
      #     '[', format(Sys.time(), '%H:%M:%S'), '] Filtering results based on ',
      #     'specified thresholds...'
      #   )
      # )

      ## create empty tibble for results
      results <- tibble::tibble(
          group = character(),
          name = character(),
          enrichment_score = numeric(),
          p_value = numeric(),
          q_value = numeric()
        )

      ## calculate statistics for enrichment scores and filter those which don't
      ## pass the specified tresholds
      for ( i in seq_len(ncol(enrichment_scores)) ) {

        ## collect enrichment scores
        temp_results <- tibble::tibble(
            group = colnames(matrix_mean_by_group)[i],
            name = rownames(enrichment_scores),
            enrichment_score = enrichment_scores[,i]
          )

        ## check how many gene sets are available
        ## ... fewer than 2 gene sets available
        if ( nrow(temp_results) < 2 ) {

          ## log message
          message(
            paste0(
              '[', format(Sys.time(), '%H:%M:%S'), '] Only 1 gene set ',
              'available, therefore p- and q-values cannot be calculated.'
            )
          )

          ## p- and q-value cannot be computed, add NA instead
          temp_results <- temp_results %>%
            dplyr::mutate(p_value = NA, q_value = NA)

        ## ... at least 2 gene sets are available
        } else if ( nrow(temp_results) >= 2 ) {

          ## log message
          # message(
          #   paste0(
          #     '[', format(Sys.time(), '%H:%M:%S'), '] Filtering results based ',
          #     'on specified thresholds...'
          #   )
          # )

          ## calculate p- and q-values
          temp_p_values <- stats::pnorm(-abs(scale(temp_results$enrichment_score)[,1]))
          temp_q_values <- suppressWarnings(qvalue::qvalue(temp_p_values, pi0 = 1, ties=min)$lfdr)

          ## assign p- and q-values and filter gene sets based on the results
          temp_results <- temp_results %>%
            dplyr::mutate(
              p_value = temp_p_values,
              q_value = temp_q_values
            ) %>%
            dplyr::filter(
              .data$p_value <= thresh_p_val,
              .data$q_value <= thresh_q_val
            )
        }

        ## merge and sort results
        results <- dplyr::bind_rows(results, temp_results) %>%
          dplyr::arrange(.data$q_value)
      }

      ## - add description, number of genes and list of genes to results
      ## - put columns in right order
      ## - rename group column
      results <- dplyr::left_join(results, gene_sets_tibble, by = 'name') %>%
        dplyr::select(c('group','name','description','length','genes',
          'enrichment_score','p_value','q_value')) %>%
        dplyr::rename(!!current_group := .data$group)

      ## factorize group column
      group_levels_with_results <- group_levels[group_levels %in% results[[ current_group ]]]
      results[[ current_group ]] <- factor(results[[ current_group ]], levels = group_levels_with_results)

      ## print number of enriched gene sets
      message(
        glue::glue(
          '[', format(Sys.time(), '%H:%M:%S'), '] {nrow(results)} gene sets ',
          'passed the thresholds across all subgroups of group `{current_group}`.'
        )
      )

      ## test if any gene sets passed the filtering
      if ( nrow(results) == 0 ) {
        results <- 'no_gene_sets_enriched'
      }

      ## add results to Seurat object
      object@misc[["enriched_pathways"]][[ name ]][[ current_group ]] <- results
    }
  }

  ##--------------------------------------------------------------------------##
  ## return Seurat object
  ##--------------------------------------------------------------------------##
  return(object)
}

