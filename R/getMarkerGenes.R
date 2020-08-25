#' Get marker genes for specified grouping variables in Seurat object.
#' @title Get marker genes for specified grouping variables in Seurat object.
#' @description This function gets marker genes for one or multiple
#' grouping variables in the meta data of the provided Seurat object.
#' @keywords Cerebro scRNAseq Seurat
#' @param object Seurat object.
#' @param assay Assay to pull transcripts counts from; defaults to 'RNA'.
#' @param organism Organism information for pulling info about presence of
#' marker genes of cell surface; can be omitted if already saved in Seurat
#' object; defaults to NULL.
#' @param groups Grouping variables (columns) in object@meta.data for which
#' marker genes should be calculated.
#' @param name Name of list that should be used to store the results in
#' object@misc$marker_genes$<name>; defaults to 'cerebro_seurat'.
#' @param only_pos Identify only over-expressed genes; defaults to TRUE.
#' @param min_pct Only keep genes that are expressed in at least n\% of current
#' group of cells, defaults to 0.70 (70\%).
#' @param thresh_logFC Only keep genes that show an average logFC of at least n;
#' defaults to 0.25.
#' @param thresh_p_val Threshold for p-value, defaults to 0.01.
#' @param test Statistical test used, defaults to 'wilcox' (Wilcoxon test).
#' @param verbose Print progress bar; defaults to TRUE.
#' @param ... Further parameters can be passed to control
#' Seurat::FindAllMakers().
#' @export
#' @return Seurat object with marker gene results for the specified grouping
#' variables stored in object@misc$marker_genes.
#' @importFrom biomaRt getBM useMart
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom Seurat FindAllMarkers Idents
#' @importFrom tidyselect all_of any_of
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.3/seurat_pbmc.rds",
#'   package = "cerebroApp"))
#' pbmc <- getMarkerGenes(
#'   object = pbmc,
#'   assay = 'RNA',
#'   organism = 'hg',
#'   groups = c('sample','seurat_clusters','cell_type_singler_blueprintencode_main'),
#'   name = 'cerebro_seurat',
#'   only_pos = TRUE,
#'   min_pct = 0.7,
#'   thresh_logFC = 0.25,
#'   thresh_p_val = 0.01,
#'   test = 'wilcox',
#'   verbose = TRUE
#' )
getMarkerGenes <- function(
  object,
  assay = 'RNA',
  organism = NULL,
  groups = NULL,
  name = 'cerebro_seurat',
  only_pos = TRUE,
  min_pct = 0.70,
  thresh_logFC = 0.25,
  thresh_p_val = 0.01,
  test = 'wilcox',
  verbose = TRUE,
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

  ## check if `counts` matrix exist in provided assay
  if ( is.null(object@assays[[assay]]@counts) ) {
    stop(
      paste0(
        '`counts` matrix could not be found in `', assay, '` assay slot of the provided Seurat object.'
      ),
      call. = FALSE
    )
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

  ## check if 'marker_genes' slot already exists and create it if not
  if ( is.null(object@misc$marker_genes) ) {
    object@misc$marker_genes <- list()
  }

  ##--------------------------------------------------------------------------##
  ## Get list of genes in cell surface through gene ontology term GO:0009986.
  ##--------------------------------------------------------------------------##

  ## check if organism is among compatible ones
  ## ... organism is not "hg" or "mm" -> not compatible
  if ( organism %in% c('hg','mm') == FALSE ) {

    ## show log message
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] No information about genes on cell surface because organism is ',
        'either not specified or not human/mouse.'
      )
    )

  ## ... organism is either "hg" or "mm" -> compatible
  } else {

    ## check which organism it is
    ## ... organism is human
    if ( organism == 'hg' || organism == 'human' ) {
      temp_attributes <- 'hgnc_symbol'
      temp_dataset <- 'hsapiens_gene_ensembl'

    ## ... organism is mouse
    } else if ( organism == 'mm' || organism == 'mouse' ) {
      temp_attributes <- 'external_gene_name'
      temp_dataset <- 'mmusculus_gene_ensembl'
    }

    ## try up to 3 times to retrieve genes in "cell surface" GO term
    attempt <- 1
    while(
      !exists('genes_on_cell_surface') &&
      attempt <= 3
    ) {
      try(
        genes_on_cell_surface <- biomaRt::getBM(
          attributes = temp_attributes,
          filters = 'go',
          values = 'GO:0009986',
          mart = biomaRt::useMart('ensembl', dataset = temp_dataset)
        )[,1]
      )
    }

    ## if genes could not be retrieved, show log message
    if ( !exists('genes_on_cell_surface') ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Genes in GO term "cell surface" (GO:0009986) could not be ',
          'retrieved, possibly due to the server not being reachable at the ',
          'moment.'
        )
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## get marker genes for each group level in every group
  ##--------------------------------------------------------------------------##

  ## create slot for results
  object@misc$marker_genes[[ name ]] <- list()

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
            'Found ', number_of_cells_without_group_assignment,
            ' cell(s) without group assignment (NA) for `', current_group,
            '`. These cells will be ignored during the analysis.'
          ),
          call. = FALSE
        )
      }
    }

    ## check number of group levels
    ## ... if only 1 group level is present, show warning and move to next
    ##     grouping variable
    if ( length(group_levels) == 1 ) {
      warning(
        paste0(
          'Only one group level found for group `', current_group,
          '`. Will skip this group and proceed to next.'
        ),
        call. = FALSE
      )

    ## ... more than 1 group level is available
    } else if ( length(group_levels) > 1 ) {

      ## log message
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] Get marker genes for ',
          length(group_levels), ' groups in `', current_group, '`...'
        )
      )

      ## set cell identities to current grouping variable
      Seurat::Idents(object) <- current_group

      ## call "FindAllMarkers()" function from Seurat
      results <- Seurat::FindAllMarkers(
        object,
        assay = assay,
        only.pos = only_pos,
        min.pct = min_pct,
        logfc.threshold = thresh_logFC,
        return.thresh = thresh_p_val,
        test.use = test,
        verbose = verbose,
        ...
      )

      ## check if marker genes were found
      ## ... no marker genes were found
      if ( nrow(results) == 0 ) {

        ## log message and set result to specific string
        message(
          paste0(
            '[', format(Sys.time(), '%H:%M:%S'),
            '] No marker genes found for any of the level of `', current_group,
            '`.'
          )
        )
        results <- 'no_markers_found'

      ## ... at least 1 marker gene was found
      } else if ( nrow(results) > 0 ) {

        ## intersect marker genes with cell surface genes if info is available
        if (
          exists('genes_on_cell_surface') &&
          "gene" %in% colnames(results)
        ) {
          results <- results %>%
            dplyr::mutate(on_cell_surface = .data$gene %in% genes_on_cell_surface)
        }
      }

      ## - try to assign the name of the current grouping variable to the first
      ##   column; not sure this will work with every test that can be selected
      ## - move "gene" column further to the front, if it exists
      if ( "cluster" %in% colnames(results) ) {
        results <- results %>%
          dplyr::rename(!!current_group := .data$cluster) %>%
          dplyr::select(tidyselect::all_of(current_group), tidyselect::any_of("gene"), dplyr::everything())
      }

      ## add results to Seurat object
      object@misc[["marker_genes"]][[ name ]][[ current_group ]] <- results
    }
  }

  ##--------------------------------------------------------------------------##
  ## return Seurat object
  ##--------------------------------------------------------------------------##
  return(object)
}
