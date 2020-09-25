#' @title
#' Get enriched pathways based on marker genes from EnrichR.
#'
#' @description
#' This function uses the enrichR API to look for enriched pathways in marker
#' gene sets of all available grouping variables.
#'
#' @param object Seurat object with marker genes calculated by
#' \code{\link{getMarkerGenes}}.
#' @param marker_genes_input Name of list of marker gene tables that will be
#' used as input. This could be the "name" parameter used in
#' \code{\link{getMarkerGenes()}}. Enriched pathways will be calculated for
#' every group level of every grouping variable. Defaults to "cerebro_seurat".
#' @param databases Which databases to query. Use enrichR::listEnrichrDbs() to
#' check what databases are available.
#' @param adj_p_cutoff Cut-off for adjusted p-value of enriched pathways;
#' defaults to 0.05,
#' @param max_terms Save only first n entries of each database; defaults to 100.
#' @param URL_API URL to send requests to (Enrichr API). Allows to overwrite
#' default URL with an alternative taken from the Enrichr website in case the
#' original is out-of-service; defaults to
#' 'http://amp.pharm.mssm.edu/Enrichr/enrich'.
#'
#' @return
#' Seurat object with Enrichr results for all provided grouping variables,
#' stored in \code{object@misc$enriched_pathways$<marker_genes_input>_enrichr}
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.3/pbmc_seurat.rds",
#'   package = "cerebroApp"))
#' pbmc <- getEnrichedPathways(
#'   object = pbmc,
#'   marker_genes_input = 'cerebro_seurat',
#'   databases = c('GO_Biological_Process_2018','GO_Cellular_Component_2018'),
#'   adj_p_cutoff = 0.01,
#'   max_terms = 100,
#'   URL_API = 'http://amp.pharm.mssm.edu/Enrichr/enrich'
#' )
#'
#' @import dplyr
#' @importFrom future.apply future_sapply
#' @importFrom rlang .data
#' @importFrom tidyselect all_of
#'
#' @export
#'
getEnrichedPathways <- function(
  object,
  marker_genes_input = 'cerebro_seurat',
  databases = c(
    'GO_Biological_Process_2018',
    'GO_Cellular_Component_2018',
    'GO_Molecular_Function_2018',
    'KEGG_2016',
    'WikiPathways_2016',
    'Reactome_2016',
    'Panther_2016',
    'Human_Gene_Atlas',
    'Mouse_Gene_Atlas'
  ),
  adj_p_cutoff = 0.05,
  max_terms = 100,
  URL_API = 'http://amp.pharm.mssm.edu/Enrichr/enrich'
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

  ## check if marker genes are present and stop if they aren't
  if (
    is.null(object@misc$marker_genes) ||
    is.null(object@misc$marker_genes[[ marker_genes_input ]]) ||
    !is.list(object@misc$marker_genes[[ marker_genes_input ]])
  ) {
    stop(
      "No marker genes found. Please run 'getMarkerGenes()' first.",
      call. = FALSE
    )
  }

  ## check if 'enriched_pathways' slot already exists and create it if not
  if ( is.null(object@misc$enriched_pathways) ) {
    object@misc$enriched_pathways <- list()
  }

  ##--------------------------------------------------------------------------##
  ## get enriched pathways for each group level in every group
  ##--------------------------------------------------------------------------##

  ## create slot for results
  object@misc[["enriched_pathways"]][[ paste0(marker_genes_input, "_enrichr") ]] <- list()

  ## get names of groups for which results seem to exist
  groups <- names(object@misc$marker_genes[[ marker_genes_input ]])

  ## log message
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Found ', length(groups),
      ' groups: ', paste0(groups, collapse = ', ')
    )
  )

  ##
  for ( i in seq_along(groups) ) {

    ## 
    current_group <- groups[i]

    ## get input data
    current_marker_genes <- object@misc$marker_genes[[ marker_genes_input ]][[ current_group ]]

    ## check input data
    ## ... no marker genes were found
    if (
      is.character(current_marker_genes) &&
      current_marker_genes == 'no_markers_found'
    ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] No marker genes for group `', current_group, '` were found. Will '
          ,'proceed with next group.'
        )
      )
      results <- 'no_markers_found'

    ## ... input is not a data frame
    } else if ( !is.data.frame(current_marker_genes) ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Data for group `', current_group, '` is not a data frame. Will ',
          'proceed with next group.'
        )
      )

    ## ... input data frame doesn't contain required "gene" column
    } else if ( "gene" %in% colnames(current_marker_genes) == FALSE ) {

      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Data for group `', current_group, '` does not contain a column ',
          'named "gene", which is required. Will proceed with next group.'
        )
      )

    ## ... input is not a data frame
    } else if ( current_group %in% colnames(current_marker_genes) == FALSE ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Cannot find column with group level information for group `',
          current_group, '`, which is expected to be named `',
          current_group, '`. Will proceed with next group.'
        )
      )

    ## ... input is a data frame and contains columns with gene and group levels
    } else {

      ## log message
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Get enriched pathways for group `', current_group, '`...'
        )
      )

      ## get group levels based on value type
      ## ... group levels are factors
      if ( is.factor(current_marker_genes[[ current_group ]]) ) {

        ## get factor levels
        group_levels <- levels(current_marker_genes[[ current_group ]])

        ## subset factor levels for those that actually exist in the data frame
        ## and therefore have at least 1 marker gene
        group_levels <- group_levels[group_levels %in% current_marker_genes[[ current_group ]]]

      ## ... group levels are characters
      } else if ( is.character(current_marker_genes[[ current_group ]]) ) {

        ## get unique values
        group_levels <- unique(current_marker_genes[[ current_group ]])
      }

      ## get results
      results <- future.apply::future_sapply(
        group_levels, USE.NAMES = TRUE, simplify = FALSE,
        future.globals = FALSE, function(x)
      {

        ## create empty list
        data_from_enrichr <- list()

        ## try max 3 times to get results from server
        attempt <- 1
        while(
          # 'Adjusted.P.value' %in% names(data_from_enrichr) == FALSE &&
          databases[1] %in% names(data_from_enrichr) == FALSE &&
          attempt <= 3
        ) {

          ## filter marker genes table for current group level
          marker_genes_current_group_level <- current_marker_genes[ current_marker_genes[[ current_group ]] == x ,]

          ## send request to server
          try(
            data_from_enrichr <- .send_enrichr_query(
              marker_genes_current_group_level$gene,
              databases = databases, URL_API = URL_API
            )
          )

          ## bump attempt counter
          attempt <- attempt + 1
        }

        ## check data from enrichr
        ## ... data is not a list, doesn't contain first database, or
        ##     'Adjusted.P.value' column is missing from first list entry
        if (
          !is.list(data_from_enrichr) ||
          databases[1] %in% names(data_from_enrichr) == FALSE ||
          'Adjusted.P.value' %in% colnames(data_from_enrichr[[1]]) == FALSE
        ) {
          message(
            paste0(
              '[', format(Sys.time(), '%H:%M:%S'),
              '] Data returned by Enrichr for subgroup `', x, '` of group `',
              current_group, '`, does not appear to be in the right format. ',
              'Will proceed with next subgroup.'
            )
          )

        ## ... data is in expected format
        } else {

          ## filter results from each database
          data_from_enrichr_merged <- sapply(names(data_from_enrichr),
            USE.NAMES = TRUE, simplify = FALSE, function(x)
          {

            ## apply cut-off of adj. p-value and add database info as column
            table_filtered <- data_from_enrichr[[ x ]] %>%
              dplyr::filter(.data$Adjusted.P.value <= adj_p_cutoff) %>%
              dplyr::mutate(db = x)

            ## check if too many entries were received
            ## ... more entries than set in "max_terms" were found
            if ( nrow(table_filtered) > max_terms ) {

              ## sort terms by adj. p-value and take top "max_terms" terms
              table_filtered <- dplyr::slice_min(table_filtered, n = max_terms, order_by = .data$Adjusted.P.value)

            ## ... no entries left after filtering
            } else if ( nrow(table_filtered) == 0 ) {
              table_filtered <- NULL
            }

            ## return results
            return(table_filtered)
          })

          ## remove databases without any enriched entries
          for ( i in names(data_from_enrichr_merged) ) {
            if ( is.null(data_from_enrichr_merged[[ i ]]) ) {
              data_from_enrichr_merged[[ i ]] <- NULL
            }
          }

          ## merge results from different databases
          data_from_enrichr_merged <- do.call(rbind, data_from_enrichr_merged)

          ## return results
          return(data_from_enrichr_merged)
        }
      })

      ## remove group levels without any enriched entry in any database
      for ( i in names(results) ) {
        if ( is.null(results[[ i ]]) ) {
          results[[ i ]] <- NULL
        }
      }

      ## check if results for all group levels are empty
      ## ... no results for any group level
      if ( length(results) == 0 ) {
        message(
          paste0(
            '[', format(Sys.time(), '%H:%M:%S'), '] 0 pathways passed the ',
            'threshold across all group levels and databases for group `', current_group, '`.'
          )
        )
        results <- 'no_pathways_found'

      ## ... results are present
      } else {

        ## add group level info as column
        for ( i in names(results) ) {
          results[[ i ]] <- dplyr::mutate(results[[ i ]], !!current_group := i)
        }

        ## - merge results from different group levels into single table
        ## - put columns in right order
        results <- do.call(rbind, results) %>%
          dplyr::select(tidyselect::all_of(current_group), 'db', dplyr::everything())

        ## factorize group levels and databases
        group_levels_with_results <- group_levels[group_levels %in% results[[ current_group ]]]
        results[[ current_group ]] <- factor(results[[ current_group ]], levels = group_levels_with_results)
        results[[ "db" ]] <- factor(results[[ "db" ]], levels = databases)

        ## log message
        message(
          paste0(
            '[', format(Sys.time(), '%H:%M:%S'), '] ', nrow(results),
            ' pathways passed the thresholds across all group levels and databases.'
          )
        )
      }
    }

    ## add results to Seurat object
    object@misc[["enriched_pathways"]][[ paste0(marker_genes_input, "_enrichr") ]][[ current_group ]] <- results
  }

  ##--------------------------------------------------------------------------##
  ## return Seurat object
  ##--------------------------------------------------------------------------##
  return(object)
}
