#' @title
#' Export Seurat object to Cerebro.
#'
#' @description
#' This function allows to export a Seurat object to visualize in Cerebro.
#'
#' @param object Seurat object.
#' @param assay Assay to pull expression values from; defaults to \code{RNA}.
#' @param slot Slot to pull expression values from; defaults to \code{data}. It
#' is recommended to use sparse data (such as log-transformed or raw counts)
#' instead of dense data (such as the \code{scaled} slot) to avoid performance
#' bottlenecks in the Cerebro interface.
#' @param file Where to save the output.
#' @param experiment_name Experiment name.
#' @param organism Organism, e.g. \code{hg} (human), \code{mm} (mouse), etc.
#' @param groups Names of grouping variables in meta data
#' (\code{object@meta.data}), e.g. \code{c("sample","cluster")}; at least one
#' must be provided; defaults to \code{NULL}.
#' @param cell_cycle Names of columns in meta data
#' (\code{object@meta.data}) that contain cell cycle information, e.g.
#' \code{c("Phase")}; defaults to \code{NULL}.
#' @param nUMI Column in \code{object@meta.data} that contains information about
#' number of transcripts per cell; defaults to \code{nUMI}.
#' @param nGene Column in \code{object@meta.data} that contains information
#' about number of expressed genes per cell; defaults to \code{nGene}.
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
#' pbmc <- readRDS(system.file("extdata/v1.3/pbmc_seurat.rds",
#'   package = "cerebroApp"))
#' exportFromSeurat(
#'   object = pbmc,
#'   file = 'pbmc_Seurat.crb',
#'   experiment_name = 'PBMC',
#'   organism = 'hg',
#'   groups = c('sample','seurat_clusters'),
#'   nUMI = 'nCount_RNA',
#'   nGene = 'nFeature_RNA',
#'   use_delayed_array = FALSE,
#'   verbose = TRUE
#' )
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
#'
exportFromSeurat <- function(
  object,
  assay = 'RNA',
  slot = 'data',
  file,
  experiment_name,
  organism,
  groups,
  cell_cycle = NULL,
  nUMI = 'nUMI',
  nGene = 'nGene',
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  verbose = FALSE
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

  ## `groups`
  if ( any(groups %in% names(object@meta.data) == FALSE ) ) {
    stop(
      paste0(
        'Some group columns could not be found in meta data: ',
        paste0(
          groups[which(groups %in% names(object@meta.data) == FALSE)],
          collapse = ', '
        )
      ),
      call. = FALSE
    )
  }

  ## `nUMI`
  if ( ( nUMI %in% names(object@meta.data) == FALSE ) ) {
    stop(
      paste0(
        'Column with number of transcripts per cell (`', nUMI,
        '`) not found in meta data.'
      ),
      call. = FALSE
    )
  }

  ## `nGene`
  if ( (nGene %in% names(object@meta.data) == FALSE ) ) {
    stop(
      paste0(
        'Column with number of expressed genes per cell (`', nGene,
        '`) not found in meta data.'
      ),
      call. = FALSE
    )
  }

  ## `cell_cycle`
  if ( any(cell_cycle %in% names(object@meta.data) == FALSE ) ) {
    stop(
      paste0(
        'Some cell cycle columns could not be found in meta data: ',
        paste0(
          cell_cycle[which(cell_cycle %in% names(object@meta.data) == FALSE)],
          collapse = ', '
        )
      ),
      call. = FALSE
    )
  }

  ## check if provided assay exists
  if ( (assay %in% names(object@assays) == FALSE ) ) {
    stop(
      paste0(
        'Specified assay `', assay, '` could not be found in provided Seurat ',
        'object.'
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
    Seurat::GetAssayData(object, assay = assay, slot = slot),
    silent = TRUE
  )

  ## check if provided slot exists in provided assay
  if ( class(expression_data) == 'try-error' ) {
    stop(
      paste0(
        'Slot `', slot, '` could not be found in `', assay, '` assay slot.'
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
  export$setExpression(expression_data)

  ##--------------------------------------------------------------------------##
  ## collect some more data if present
  ##--------------------------------------------------------------------------##

  ## date of analysis
  if ( !is.null(object@misc$experiment$date_of_analysis) ) {
    export$addExperiment('date_of_analysis', object@misc$experiment$date_of_analysis)
  }

  ## date of export
  export$addExperiment('date_of_export', Sys.Date())

  ## `parameters`
  if ( !is.null(object@misc$parameters) ) {
    for ( i in seq_along(object@misc$parameters) ) {
      name <- names(object@misc$parameters)[i]
      export$addParameters(
        name,
        object@misc$parameters[[name]]
      )
    }
  }

  ## `technical_info`
  if ( !is.null(object@misc$technical_info) ) {
    for ( i in seq_along(object@misc$technical_info) ) {
      export$addTechnicalInfo(
        names(object@misc$technical_info)[i],
        object@misc$technical_info[[i]]
      )
    }
  }

  ## `gene_lists`
  if ( !is.null(object@misc$gene_lists) ) {
    for ( i in seq_along(object@misc$gene_lists) ) {
      export$addGeneList(
        names(object@misc$gene_lists)[i],
        object@misc$gene_lists[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## prepare meta data
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
    "cell_barcode" = Seurat::Cells(object),
    stringsAsFactors = FALSE
  )

  ##--------------------------------------------------------------------------##
  ## add grouping variables, factorize if necessary
  ##--------------------------------------------------------------------------##

  ## go through grouping variables
  for ( i in groups ) {

    ## check content of column in meta data
    ## ... content not factorized
    if (
      !is.factor(object@meta.data[[i]]) &&
      is.character(object@meta.data[[i]])
    ) {

      ## get all values and unique values (sorted, which removes NA)
      values <- object@meta.data[[i]]
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
      is.factor(object@meta.data[[i]]) &&
      any(is.na(object@meta.data[[i]])) &&
      'NA' %in% levels(object@meta.data[[i]]) == FALSE
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
      levels <- levels(object@meta.data[[i]])
      values <- as.character(object@meta.data[[i]])
      values[is.na(values)] <- 'N/A'
      values <- factor(values, levels = c(levels, 'N/A'))
      temp_meta_data[[i]] <- values

    ## ... none of the above
    } else {

      ## copy content to meta data
      temp_meta_data[[i]] <- object@meta.data[[i]]
    }
  }

  ## number of transcripts and expressed genes
  temp_meta_data[["nUMI"]] = object@meta.data[[nUMI]]
  temp_meta_data[["nGene"]] = object@meta.data[[nGene]]

  ## rest of meta data
  meta_data_columns <- names(object@meta.data)
  meta_data_columns <- meta_data_columns[-which(meta_data_columns %in% groups)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == nUMI)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == nGene)]

  ##--------------------------------------------------------------------------##
  ## cell cycle
  ##--------------------------------------------------------------------------##
  if (
    !is.null(cell_cycle) &&
    length(cell_cycle) > 0
  ) {
    for ( i in cell_cycle ) {
      if ( is.factor(object@meta.data[[i]]) ) {
        tmp_names <- levels(object@meta.data[[i]])
      } else {
        tmp_names <- unique(object@meta.data[[i]])
      }
      # colData(export$expression)[[i]] <- factor(object@meta.data[[i]], levels = tmp_names)
      temp_meta_data[[i]] <- factor(object@meta.data[[i]], levels = tmp_names)
    }
    meta_data_columns <- meta_data_columns[-which(meta_data_columns %in% cell_cycle)]
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
      # colData(export$expression)[[i]] <- object@meta.data[[i]]
      temp_meta_data[[i]] <- object@meta.data[[i]]
    }
  }

  ## make column names in meta data unique (if necessary)
  # colnames(colData(export$expression)) <- make.unique(colnames(colData(export$expression)))
  colnames(temp_meta_data) <- make.unique(colnames(temp_meta_data))

  ##--------------------------------------------------------------------------##
  ## add meta data
  ##--------------------------------------------------------------------------##
  export$setMetaData(temp_meta_data)

  ##--------------------------------------------------------------------------##
  ## add grouping variables and cell cycle columns
  ##--------------------------------------------------------------------------##
  for ( i in groups ) {
    export$addGroup(i, levels(temp_meta_data[[i]]))
  }

  if (
    !is.null(cell_cycle) &&
    length(cell_cycle) > 0
  ) {
    export$setCellCycle(cell_cycle)
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
  projections_available <- names(object@reductions)
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
    # SingleCellExperiment::reducedDims(export$expression)[[projections_available]] <- as.data.frame(
    #   object@reductions[[projections_available]]@cell.embeddings
    # )
    export$addProjection(
      projections_available,
      as.data.frame(object@reductions[[projections_available]]@cell.embeddings)
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
      # SingleCellExperiment::reducedDims(export$expression)[[projection]] <- as.data.frame(
      #   object@reductions[[projection]]@cell.embeddings
      # )
      export$addProjection(
        projection,
        as.data.frame(object@reductions[[projection]]@cell.embeddings)
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## group trees
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$trees) ) {
    ## check if it's a list
    if ( !is.list(object@misc$trees) ) {
      stop(
        '`object@misc$trees` is not a list.',
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
    for ( i in seq_along(object@misc$trees) ) {
      export$addTree(
        names(object@misc$trees)[i],
        object@misc$trees[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## most expressed genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$most_expressed_genes) ) {
    ## check if it's a list
    if ( !is.list(object@misc$most_expressed_genes) ) {
      stop(
        '`object@misc$most_expressed_genes` is not a list.',
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
    for ( i in seq_along(object@misc$most_expressed_genes) ) {
      export$addMostExpressedGenes(
        names(object@misc$most_expressed_genes)[i],
        object@misc$most_expressed_genes[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## marker genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$marker_genes) ) {
    ## check if it's a list
    if ( !is.list(object@misc$marker_genes) ) {
      stop(
        '`object@misc$marker_genes` is not a list.',
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
    for ( i in seq_along(object@misc$marker_genes) ) {
      method <- names(object@misc$marker_genes)[i]
      ## for each group
      for ( j in seq_along(object@misc$marker_genes[[method]]) ) {
        if ( is.list(object@misc$marker_genes[[method]][j]) ) {
          group <- names(object@misc$marker_genes[[method]])[j]
          export$addMarkerGenes(
            method,
            group,
            object@misc$marker_genes[[method]][[group]]
          )
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## enriched pathways
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$enriched_pathways) ) {
    ## check if it's a list
    if ( !is.list(object@misc$enriched_pathways) ) {
      stop(
        '`object@misc$enriched_pathways` is not a list.',
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
    for ( i in seq_along(object@misc$enriched_pathways) ) {
      method <- names(object@misc$enriched_pathways)[i]
      ## for each group
      for ( j in seq_along(object@misc$enriched_pathways[[method]]) ) {
        if ( is.list(object@misc$enriched_pathways[[method]][j]) ) {
          group <- names(object@misc$enriched_pathways[[method]])[j]
          export$addEnrichedPathways(
            method,
            group,
            object@misc$enriched_pathways[[method]][[group]]
          )
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## trajectories
  ##--------------------------------------------------------------------------##
  if ( length(object@misc$trajectories) == 0 ) {
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
          paste(names(object@misc$trajectories$monocle2), collapse = ', ')
        )
      )
    }
    ## for each method
    for ( i in seq_along(object@misc$trajectories) ) {
      method <- names(object@misc$trajectories)[i]
      if ( method == 'monocle2' ) {
        ## for each trajectory
        for ( j in seq_along(object@misc$trajectories[[i]]) ) {
          export$addTrajectory(
            method,
            names(object@misc$trajectories[[i]])[j],
            object@misc$trajectories[[i]][[j]]
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
    !is.null(object@misc$extra_material) &&
    is.list(object@misc$extra_material) &&
    length(object@misc$extra_material) > 0
  ) {

    if ( verbose ) {
      message(
        glue::glue(
          '[{format(Sys.time(), "%H:%M:%S")}] Found extra material to export...'
        )
      )
    }

    ## go through categories in `extra_material` slot
    for ( category in names(object@misc$extra_material) ) {

      ## do this if category is `tables`
      if ( category == 'tables' ) {

        ## go through tables
        for ( i in seq_along(object@misc$extra_material$tables) ) {

          ## export table
          export$addExtraMaterial(
            category = 'tables',
            name = names(object@misc$extra_material$tables)[i],
            content = object@misc$extra_material$tables[[i]]
          )
        }

      ## do this if category is `plots`
      } else if ( category == 'plots' ) {

        ## go through tables
        for ( i in seq_along(object@misc$extra_material$plots) ) {

          ## export table
          export$addExtraMaterial(
            category = 'plots',
            name = names(object@misc$extra_material$plots)[i],
            content = object@misc$extra_material$plots[[i]]
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
