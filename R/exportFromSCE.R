#' @title
#' Export SingleCellExperiment (SCE) object to Cerebro.
#'
#' @description
#' This function allows to export a \code{SingleCellExperiment} (\code{SCE})
#' object to visualize in Cerebro.
#'
#' @param object \code{SingleCellExperiment} (\code{SCE}) object.
#' @param assay Assay to pull expression values from; defaults to
#' \code{logcounts}. It is recommended to use sparse data (such as
#' log-transformed or raw counts) instead of dense data (such as the 'scaled'
#' slot) to avoid performance bottlenecks in the Cerebro interface.
#' @param file Where to save the output.
#' @param experiment_name Experiment name.
#' @param organism Organism, e.g. \code{hg} (human), \code{mm} (mouse), etc.
#' @param columns_groups Names of grouping variables in meta data
#' (\code{colData(object)}), e.g. \code{c("sample","cluster")}; at least one
#' must be provided; defaults to \code{NULL}.
#' @param column_nUMI Column in \code{colData(object)} that contains information
#' about number of transcripts per cell; defaults to \code{nUMI}.
#' @param column_nGene Column in \code{colData(object)} that contains
#' information about number of expressed genes per cell; defaults to
#' \code{nGene}.
#' @param columns_cell_cycle Names of columns in meta data
#' (\code{colData(object)}) that contain cell cycle information, e.g.
#' \code{c("Phase")}; defaults to \code{NULL}.
#' @param add_all_meta_data If set to \code{TRUE}, all further meta data columns
#' will be extracted as well.
#' @param use_delayed_array When set to \code{TRUE}, the expression matrix will
#' be stored as an \code{RleMatrix} (see \code{DelayedArray} package). This can
#' be useful for very large data sets, as the matrix won't be loaded into memory
#' and instead values will be read from the disk directly, at the cost of
#' performance. Note that it is necessary to install the \code{DelayedArray}
#' package. If set to \code{FALSE} (default), the expression matrix will be
#' copied from the input object as is. It is recommended to use a sparse format,
#' such as \code{dgCMatrix} from the \code{Matrix} package.
#' @param verbose Set this to \code{TRUE} if you want additional log messages;
#' defaults to \code{FALSE}.
#'
#' @return
#' No data returned.
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.3/pbmc_SCE.rds",
#'   package = "cerebroApp"))
#' exportFromSCE(
#'   object = pbmc,
#'   file = 'pbmc_SCE.crb',
#'   experiment_name = 'PBMC',
#'   organism = 'hg',
#'   columns_groups = c('sample','cluster'),
#'   column_nUMI = 'nUMI',
#'   column_nGene = 'nGene',
#'   use_delayed_array = FALSE,
#'   verbose = TRUE
#' )
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
#'
exportFromSCE <- function(
  object,
  assay = 'logcounts',
  file,
  experiment_name,
  organism,
  columns_groups,
  column_nUMI = 'nUMI',
  column_nGene = 'nGene',
  columns_cell_cycle = NULL,
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  verbose = FALSE
) {

  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  ## check if provided object is of class "SingleCellExperiment"
  if ( class(object) != "SingleCellExperiment" ) {
    stop(
      paste0(
        "Provided object is of class `", class(object), "` but must be of class 'SingleCellExperiment'."
      ),
      call. = FALSE
    )
  }

  ## check if SingleCellExperiment is installed
  if ( !requireNamespace("SingleCellExperiment", quietly = TRUE) ) {
    stop(
      "The 'SingleCellExperiment' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## make functions from SingleCellExperiment package available for next tests
  require('SingleCellExperiment')

  ## `columns_groups`
  if ( any(columns_groups %in% names(colData(object)) == FALSE ) ) {
    stop(
      paste0(
        'Some group columns could not be found in meta data: ',
        paste0(
          columns_groups[which(columns_groups %in% names(colData(object)) == FALSE)],
          collapse = ', '
        )
      ),
      call. = FALSE
    )
  }

  ## `column_nUMI`
  if ( (column_nUMI %in% names(colData(object)) == FALSE ) ) {
    stop(
      paste0(
        'Column with number of transcripts per cell (`', column_nUMI,
        '`) not found in meta data.'
      ),
      call. = FALSE
    )
  }

  ## `column_nGene`
  if ( (column_nGene %in% names(colData(object)) == FALSE ) ) {
    stop(
      paste0(
        'Column with number of expressed genes per cell (`', column_nGene,
        '`) not found in meta data.'
      ),
      call. = FALSE
    )
  }

  ## `columns_cell_cycle`
  if ( any(columns_cell_cycle %in% names(colData(object)) == FALSE ) ) {
    stop(
      paste0(
        'Some cell cycle columns could not be found in meta data: ',
        paste0(
          columns_cell_cycle[which(columns_cell_cycle %in% names(colData(object)) == FALSE)],
          collapse = ', '
        )
      ),
      call. = FALSE
    )
  }

  ## check if provided assay exists
  if ( (assay %in% names(assays(object)) == FALSE ) ) {
    stop(
      glue::glue(
        'Specified assay `{assay}` could not be found in provided SCE object.'
      ),
      call. = FALSE
    )
  }

  ##--------------------------------------------------------------------------##
  ## initialize Cerebro object
  ##--------------------------------------------------------------------------##
  if ( verbose ) {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] Initializing Cerebro object...'
      )
    )
  } else {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] Start collecting data...'
      )
    )
  }

  ## create new Cerebro object
  export <- Cerebro_v1.3$new()

  ## add experiment name
  export$addExperiment('experiment_name', experiment_name)

  ## add organism
  export$addExperiment('organism', organism)

  ## add cerebroApp version
  export$setVersion(utils::packageVersion('cerebroApp'))

  ##--------------------------------------------------------------------------##
  ## add transcript counts
  ##--------------------------------------------------------------------------##

  ## get expression data
  expression_data <- try(
    assay(object, name = assay),
    silent = TRUE
  )

  ## check if provided slot exists in provided assay
  if ( class(expression_data) == 'try-error' ) {
    stop(
      paste0(
        'Assay `', assay, '` could not be found.'
      ),
      call. = FALSE
    )
  }

  ## convert expression data to "RleArray" if requested, if it is "dgCMatrix" or
  ## "matrix" format, and if the "DelayedArray" package is available
  if (
    use_delayed_array == TRUE &&
    class(expression_data) %in% c('matrix','dgCMatrix') &&
    requireNamespace("DelayedArray", quietly = TRUE)
  ) {
    if ( verbose ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] Storing expression data as ',
          'DelayedArray...'
        )
      )
    }
    require('DelayedArray')
    expression_data <- as(expression_data, "RleArray")
  }

  ## add expression data
  export$setExpression(assay(object, assay))

  ##--------------------------------------------------------------------------##
  ## collect some more data if present
  ##--------------------------------------------------------------------------##

  ## date of analysis
  if ( !is.null(object@metadata$experiment$date_of_analysis) ) {
    export$addExperiment('date_of_analysis', object@metadata$experiment$date_of_analysis)
  }

  ## date of export
  export$addExperiment('date_of_export', Sys.Date())

  ## `parameters`
  if ( !is.null(object@metadata$parameters) ) {
    for ( i in seq_along(object@metadata$parameters) ) {
      name <- names(object@metadata$parameters)[i]
      export$addParameters(
        name,
        object@metadata$parameters[[name]]
      )
    }
  }

  ## `technical_info`
  if ( !is.null(object@metadata$technical_info) ) {
    for ( i in seq_along(object@metadata$technical_info) ) {
      export$addTechnicalInfo(
        names(object@metadata$technical_info)[i],
        object@metadata$technical_info[[i]]
      )
    }
  }

  ## `gene_lists`
  if ( !is.null(object@metadata$gene_lists) ) {
    for ( i in seq_along(object@metadata$gene_lists) ) {
      export$addGeneList(
        names(object@metadata$gene_lists)[i],
        object@metadata$gene_lists[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## meta data
  ##--------------------------------------------------------------------------##
  if ( verbose ) {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] Collecting available meta data...'
      )
    )
  }

  ## cell barcodes
  temp_meta_data <- data.frame(
    "cell_barcode" = colnames(object),
    stringsAsFactors = FALSE
  )

  ##--------------------------------------------------------------------------##
  ## add grouping variables, factorize if necessary
  ##--------------------------------------------------------------------------##

  ## go through grouping variables
  for ( i in columns_groups ) {

    ## check content of column in meta data
    ## ... content not factorized
    if (
      !is.factor(colData(object)[[i]]) &&
      is.character(colData(object)[[i]])
    ) {

      ## get all values and unique values (sorted, which removes NA)
      values <- colData(object)[[i]]
      levels <- sort(unique(values), na.last = NA)

      ## check if there are NA values; if so, change NA values to 'N/A' and add
      ## 'N/A' to levels
      if ( any(is.na(values)) ) {
        values[is.na(values)] <- 'N/A'
        levels <- c(levels, 'N/A')
      }

      ## factorize values
      temp_meta_data[[i]] <- factor(values, levels = levels)

    ## ... content is factorized but there are NA values and NA is not among the
    ##     factor levels
    } else if (
      is.factor(colData(object)[[i]]) &&
      any(is.na(colData(object)[[i]])) &&
      'NA' %in% levels(colData(object)[[i]]) == FALSE
    ) {

      ## print log message
      if ( verbose ) {
        message(
          glue::glue(
            '[{format(Sys.time(), "%H:%M:%S")}] Adding `NA` to factor levels ',
            'of group `{i}`...'
          )
        )
      }

      ## add 'N/A' to factor levels for NA values
      levels <- levels(colData(object)[[i]])
      values <- as.character(colData(object)[[i]])
      values[is.na(values)] <- 'N/A'
      values <- factor(values, levels = c(levels, 'N/A'))
      temp_meta_data[[i]] <- values

    ## ... none of the above
    } else {

      ## copy content to meta data
      temp_meta_data[[i]] <- colData(object)[[i]]
    }
  }

  ## number of transcripts and expressed genes
  temp_meta_data[["nUMI"]] = colData(object)[[column_nUMI]]
  temp_meta_data[["nGene"]] = colData(object)[[column_nGene]]

  ## rest of meta data
  meta_data_columns <- names(colData(object))
  meta_data_columns <- meta_data_columns[-which(meta_data_columns %in% columns_groups)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_nUMI)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_nGene)]

  ##--------------------------------------------------------------------------##
  ## cell cycle
  ##--------------------------------------------------------------------------##
  if (
    !is.null(columns_cell_cycle) &&
    length(columns_cell_cycle) > 0
  ) {
    for ( i in columns_cell_cycle ) {
      if ( is.factor(colData(object)[[i]]) ) {
        tmp_names <- levels(colData(object)[[i]])
      } else {
        tmp_names <- unique(colData(object)[[i]])
      }
      # colData(export$expression)[[i]] <- factor(colData(object)[[i]], levels = tmp_names)
      temp_meta_data[[i]] <- factor(colData(object)[[i]], levels = tmp_names)
    }
    meta_data_columns <- meta_data_columns[-which(meta_data_columns %in% columns_cell_cycle)]
  }

  ##--------------------------------------------------------------------------##
  ## add all other meta data if specified
  ##--------------------------------------------------------------------------##
  if ( add_all_meta_data == TRUE ) {
    if ( verbose ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Extracting all meta data columns...'
        )
      )
    }
    for ( i in meta_data_columns ) {
      # colData(export$expression)[[i]] <- colData(object)[[i]]
      temp_meta_data[[i]] <- colData(object)[[i]]
    }
  }

  ## make column names in meta data unique (if necessary)
  # colnames(colData(export$expression)) <- make.unique(colnames(colData(export$expression)))
  colnames(temp_meta_data) <- make.unique(colnames(temp_meta_data))

  ##--------------------------------------------------------------------------##
  ## add meta data to Cerebro object
  ##--------------------------------------------------------------------------##
  export$setMetaData(temp_meta_data)

  ##--------------------------------------------------------------------------##
  ## add grouping variables and cell cycle columns
  ##--------------------------------------------------------------------------##
  for ( i in columns_groups ) {
    export$addGroup(i, levels(temp_meta_data[[i]]))
  }

  if (
    !is.null(columns_cell_cycle) &&
    length(columns_cell_cycle) > 0
  ) {
    export$setCellCycle(columns_cell_cycle)
  }

  ##--------------------------------------------------------------------------##
  ## projections
  ##--------------------------------------------------------------------------##
  if ( verbose ) {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Extracting dimensional reductions...'
      )
    )
  }
  projections <- list()
  projections_available <- names(SingleCellExperiment::reducedDims(object))
  projections_available_pca <- projections_available[grep(
    projections_available, pattern = 'pca', ignore.case = TRUE, invert = FALSE
  )]
  projections_available_non_pca <- projections_available[grep(
    projections_available, pattern = 'pca', ignore.case = TRUE, invert = TRUE
  )]
  if ( length(projections_available) == 0 ) {
    stop('No dimensional reductions available.', call. = FALSE)
  } else if (
    length(projections_available) == 1 &&
    length(projections_available_pca) == 1 )
  {
    SingleCellExperiment::reducedDims(export$expression)[[projections_available]] <- SingleCellExperiment::reducedDims(object)[[projections_available]]
    export$addProjection(
      projections_available,
      SingleCellExperiment::reducedDims(object)[[projections_available]]
    )
    warning(
      paste0(
        'Warning: Only PCA as dimensional reduction found, will export ',
        'first and second principal components. Consider using tSNE and/or ',
        'UMAP instead.'
      )
    )
  } else if ( length(projections_available_non_pca) >= 1 ) {
    if ( verbose ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] ',
          'Will export the following dimensional reductions: ',
          paste(projections_available_non_pca, collapse = ', ')
        )
      )
    }
    for ( projection in projections_available_non_pca ) {
      # SingleCellExperiment::reducedDims(export$expression)[[projection]] <- SingleCellExperiment::reducedDims(object)[[projection]]
      export$addProjection(
        projection,
        SingleCellExperiment::reducedDims(object)[[projection]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## group trees
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@metadata$trees) ) {
    ## check if it's a list
    if ( !is.list(object@metadata$trees) ) {
      stop(
        '`object@metadata$trees` is not a list.',
        call. = FALSE
      )
    }
    if ( verbose ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] Extracting trees...'
        )
      )
    }
    for ( i in seq_along(object@metadata$trees) ) {
      export$addTree(
        names(object@metadata$trees)[i],
        object@metadata$trees[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## most expressed genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@metadata$most_expressed_genes) ) {
    ## check if it's a list
    if ( !is.list(object@metadata$most_expressed_genes) ) {
      stop(
        '`object@metadata$most_expressed_genes` is not a list.',
        call. = FALSE
      )
    }
    if ( verbose ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Extracting tables of most expressed genes...'
        )
      )
    }
    for ( i in seq_along(object@metadata$most_expressed_genes) ) {
      export$addMostExpressedGenes(
        names(object@metadata$most_expressed_genes)[i],
        object@metadata$most_expressed_genes[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## marker genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@metadata$marker_genes) ) {
    ## check if it's a list
    if ( !is.list(object@metadata$marker_genes) ) {
      stop(
        '`object@metadata$marker_genes` is not a list.',
        call. = FALSE
      )
    }
    if ( verbose ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Extracting tables of marker genes...'
        )
      )
    }
    ## for each method
    for ( i in seq_along(object@metadata$marker_genes) ) {
      method <- names(object@metadata$marker_genes)[i]
      ## for each group
      for ( j in seq_along(object@metadata$marker_genes[[method]]) ) {
        if ( is.list(object@metadata$marker_genes[[method]][j]) ) {
          group <- names(object@metadata$marker_genes[[method]])[j]
          export$addMarkerGenes(
            method,
            group,
            object@metadata$marker_genes[[method]][[group]]
          )
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## enriched pathways
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@metadata$enriched_pathways) ) {
    ## check if it's a list
    if ( !is.list(object@metadata$enriched_pathways) ) {
      stop(
        '`object@metadata$enriched_pathways` is not a list.',
        call. = FALSE
      )
    }
    if ( verbose ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Extracting pathway enrichment results...'
        )
      )
    }
    ## for each method
    for ( i in seq_along(object@metadata$enriched_pathways) ) {
      method <- names(object@metadata$enriched_pathways)[i]
      ## for each group
      for ( j in seq_along(object@metadata$enriched_pathways[[method]]) ) {
        if ( is.list(object@metadata$enriched_pathways[[method]][j]) ) {
          group <- names(object@metadata$enriched_pathways[[method]])[j]
          export$addEnrichedPathways(
            method,
            group,
            object@metadata$enriched_pathways[[method]][[group]]
          )
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## trajectories
  ##--------------------------------------------------------------------------##
  if ( length(object@metadata$trajectories) == 0 ) {
    if ( verbose ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] No trajectories to extract...'
        )
      )
    }
  } else {
    if ( verbose ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] ',
          # 'Extracting trajectories...'
          'Will export the following trajectories: ',
          paste(names(object@metadata$trajectories$monocle2), collapse = ', ')
        )
      )
    }
    ## for each method
    for ( i in seq_along(object@metadata$trajectories) ) {
      method <- names(object@metadata$trajectories)[i]
      if ( method == 'monocle2' ) {
        ## for each trajectory
        for ( j in seq_along(object@metadata$trajectories[[i]]) ) {
          export$addTrajectory(
            method,
            names(object@metadata$trajectories[[i]])[j],
            object@metadata$trajectories[[i]][[j]]
          )
        }
      } else {
        warning(
          paste0(
            'Warning: Skipping trajectories of method `', method, '`. At the ',
            'moment, only trajectories generated with Monocle 2 (`monocle2`) ',
            'are supported.'
          )
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## extra material
  ##
  ## currently, only tables can be exported
  ##--------------------------------------------------------------------------##

  ## define valid categories
  valid_categories <- c('tables')

  ## check of extra material exists, that it is in list format, and that the
  ## list is not empty
  if (
    !is.null(object@metadata$extra_material) &&
    is.list(object@metadata$extra_material) &&
    length(object@metadata$extra_material) > 0
  ) {

    if ( verbose ) {
      message(
        glue::glue(
          '[{format(Sys.time(), "%H:%M:%S")}] Found extra material to export...'
        )
      )
    }

    ## go through categories in `extra_material` slot
    for ( category in names(object@metadata$extra_material) ) {

      ## do this if category is `tables`
      if ( category == 'tables' ) {

        ## go through tables
        for ( i in seq_along(object@metadata$extra_material$tables) ) {

          ## export table
          export$addExtraMaterial(
            category = 'tables',
            name = names(object@metadata$extra_material$tables)[i],
            content = object@metadata$extra_material$tables[[i]]
          )
        }

      ## do this if category is `plots`
      } else if ( category == 'plots' ) {

        ## go through tables
        for ( i in seq_along(object@metadata$extra_material$plots) ) {

          ## export table
          export$addExtraMaterial(
            category = 'plots',
            name = names(object@metadata$extra_material$plots)[i],
            content = object@metadata$extra_material$plots[[i]]
          )
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## show overview of Cerebro object
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] ',
      'Overview of Cerebro object:\n'
    )
  )
  export$print()

  ##--------------------------------------------------------------------------##
  ## save Cerebro object to disk
  ##--------------------------------------------------------------------------##

  ## check if output directory exists and create it if not
  if ( !file.exists(dirname(file)) ) {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Creating output directory...'
      )
    )
    dir.create(dirname(file), showWarnings = FALSE)
  }

  ## log message
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Saving Cerebro object to: ', file
    )
  )

  ## save file
  saveRDS(export, file)

  ## log message
  ## ... writing to file was successful
  if ( file.exists(file) ) {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] Done!'
      )
    )
  ## ... target file doesn't exist
  } else {
    stop(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] Something went wrong while ',
        'saving the file.'
      ),
      .call = FALSE
    )
  }
}
