#' Export SingleCellExperiment (SCE) object to Cerebro.
#' @title Export SingleCellExperiment (SCE) object to Cerebro.
#' @description This function allows to export a SingleCellExperiment (SCE)
#' object to visualize in Cerebro.
#' @keywords Cerebro scRNAseq SingleCellExperiment SCE
#' @param object SingleCellExperiment (SCE) object.
#' @param assay Assay to pull expression values from; defaults to 'logcounts'.
#' It is recommended to use sparse data (such as log-transformed or raw counts)
#' instead of dense data (such as the 'scaled' slot) to avoid performance
#' bottlenecks in the Cerebro interface.
#' @param file Where to save the output.
#' @param experiment_name Experiment name.
#' @param organism Organism, e.g. hg (human), mm (mouse), etc.
#' @param columns_groups Names of grouping variables in meta data
#' (colData(object)), e.g. c("sample","cluster"); at least one must be
#' provided; defaults to NULL.
#' @param column_nUMI Column in colData(object) that contains information about
#' number of transcripts per cell; defaults to 'nUMI'.
#' @param column_nGene Column in colData(object) that contains information about
#' number of expressed genes per cell; defaults to 'nGene'.
#' @param columns_cell_cycle Names of columns in meta data (object@meta.data)
#' that contain cell cycle information.e.g. c("Phase"); defaults to NULL.
#' @param add_all_meta_data If set to TRUE, all further meta data columns will
#' be extracted as well.
#' @export
#' @return No data returned.
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.2/SCE_pbmc.rds",
#'   package = "cerebroApp"))
#' exportFromSCE(
#'   object = pbmc,
#'   file = 'pbmc_SCE.crb',
#'   experiment_name = 'PBMC',
#'   organism = 'hg',
#'   columns_groups = c('sample','cluster'),
#'   column_nUMI = 'nCount_RNA',
#'   column_nGene = 'nFeature_RNA',
#'   columns_cell_cycle = c('Phase')
#' )
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
  add_all_meta_data = TRUE
) {
  ##--------------------------------------------------------------------------##
  ## check provided parameters
  ##--------------------------------------------------------------------------##
  ## check if SingleCellExperiment is installed
  if (!requireNamespace("SingleCellExperiment", quietly = TRUE))
  {
    stop(
      paste0(
        "Package 'SingleCellExperiment' is needed for this function to work. ",
        "Please install it."
      ),
      call. = FALSE
    )
  }
  ## check if provided object is a SingleCellExperiment object
  if ( class(object) != 'SingleCellExperiment' )
  {
    stop(
      paste0(
        "The provided object (class `", class(object), "`) does not look ",
        "like a SingleCellExperiment object."
      ),
      call. = FALSE
    )
  }
  ## `columns_groups`
  if ( any(columns_groups %in% names(colData(object)) == FALSE ) )
  {
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
  if ( (column_nUMI %in% names(colData(object)) == FALSE ) )
  {
    stop(
      paste0(
        'Column with number of transcripts per cell (`', column_nUMI,
        '`) not found in meta data.'
      ),
      call. = FALSE
    )
  }
  ## `column_nGene`
  if ( (column_nGene %in% names(colData(object)) == FALSE ) )
  {
    stop(
      paste0(
        'Column with number of expressed genes per cell (`', column_nGene,
        '`) not found in meta data.'
      ),
      call. = FALSE
    )
  }
  ## `columns_cell_cycle`
  if ( any(columns_cell_cycle %in% names(colData(object)) == FALSE ) )
  {
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
  ##--------------------------------------------------------------------------##
  ## initialize export object with expression data
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Initializing Cerebro object...'
    )
  )
  ## check if provided assay exists
  if ( (assay %in% names(assays(object)) == FALSE ) )
  {
    stop(
      paste0(
        'Specified assay `', assay, '` could not be found in provided SCE ',
        'object.'
      ),
      call. = FALSE
    )
  }
  ## create new Cerebro object
  export <- Cerebro$new()

  ## add experiment name
  export$addExperiment('experiment_name', 'pbmc_Seurat')

  ## add organism
  export$addExperiment('organism', 'hg')

  ## add cerebroApp version
  export$setVersion(utils::packageVersion('cerebroApp'))

  ## add expression data
  export$expression <- SingleCellExperiment::SingleCellExperiment(
    assays = list(expression = assay(object, assay))
  )

  ##--------------------------------------------------------------------------##
  ## collect some more data if present
  ##--------------------------------------------------------------------------##
  ## data of analysis
  export$addExperiment('date_of_analysis', object@metadata$experiment$date_of_analysis)

  ## date of export
  export$addExperiment('date_of_export', Sys.Date())

  ## `parameters`
  if ( !is.null(object@metadata$parameters) )
  {
    for ( i in seq(length(object@metadata$parameters)) )
    {
      name <- names(object@misc$parameters)[i]
      export$addParameters(
        name,
        object@metadata$parameters[[name]]
      )
    }
  }

  ## `technical_info`
  if ( !is.null(object@metadata$technical_info) )
  {
    for ( i in seq(length(object@metadata$technical_info)) )
    {
      export$addTechnicalInfo(
        names(object@metadata$technical_info)[i],
        object@metadata$technical_info[[i]]
      )
    }
  }

  ## `gene_lists`
  if ( !is.null(object@metadata$gene_lists) )
  {
    for ( i in seq(length(object@metadata$gene_lists)) )
    {
      export$addGeneList(
        names(object@metadata$parameters)[i],
        object@metadata$gene_lists[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## meta data
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Collecting available meta data...'
    )
  )

  ## cell barcodes
  colData(export$expression)$cell_barcode <- colnames(object)

  ## factorize group variables and add groups
  for ( i in columns_groups )
  {
    if ( is.factor(colData(object)[[i]]) )
    {
      tmp_names <- levels(colData(object)[[i]])
    } else
    {
      tmp_names <- unique(colData(object)[[i]])
    }
    colData(export$expression)[[i]] <- factor(colData(object)[[i]], levels = tmp_names)
    export$addGroup(i, tmp_names)
  }

  ## number of transcripts and expressed genes
  colData(export$expression)$nUMI = colData(object)[[column_nUMI]]
  colData(export$expression)$nGene = colData(object)[[column_nGene]]

  ## rest of meta data
  meta_data_columns <- names(colData(object))
  meta_data_columns <- meta_data_columns[-which(meta_data_columns %in% columns_groups)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_nUMI)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_nGene)]

  ##--------------------------------------------------------------------------##
  ## cell cycle
  ##--------------------------------------------------------------------------##
  if ( !is.null(columns_cell_cycle) && length(columns_cell_cycle) > 0 )
  {
    for ( i in columns_cell_cycle )
    {
      if ( is.factor(colData(object)[[i]]) )
      {
        tmp_names <- levels(colData(object)[[i]])
      } else
      {
        tmp_names <- unique(colData(object)[[i]])
      }
      colData(export$expression)[[i]] <- factor(colData(object)[[i]], levels = tmp_names)
    }
    export$setCellCycle(columns_cell_cycle)
    meta_data_columns <- meta_data_columns[-which(meta_data_columns %in% columns_cell_cycle)]
  }

  ##--------------------------------------------------------------------------##
  ## add all other meta data if specified
  ##--------------------------------------------------------------------------##
  if ( add_all_meta_data == TRUE )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Extracting all meta data columns...'
      )
    )
    for ( i in meta_data_columns )
    {
      colData(export$expression)[[i]] <- colData(object)[[i]]
    }
  }

  ## make column names in meta data unique (if necessary)
  colnames(colData(export$expression)) <- make.unique(colnames(colData(export$expression)))

  ##--------------------------------------------------------------------------##
  ## most expressed genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@metadata$most_expressed_genes) )
  {
    ## check if it's a list
    if ( !is.list(object@metadata$most_expressed_genes) )
    {
      stop(
        '`object@metadata$most_expressed_genes` is not a list.',
        call. = FALSE
      )
    }
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Extracting tables of most expressed genes...'
      )
    )
    for ( i in seq(length(object@metadata$most_expressed_genes)) )
    {
      export$addMostExpressedGenes(
        names(object@metadata$most_expressed_genes)[i],
        object@metadata$most_expressed_genes[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## marker genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@metadata$marker_genes) )
  {
    ## check if it's a list
    if ( !is.list(object@metadata$marker_genes) )
    {
      stop(
        '`object@metadata$marker_genes` is not a list.',
        call. = FALSE
      )
    }
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Extracting tables of marker genes...'
      )
    )
    ## for each method
    for ( i in seq(length(object@metadata$marker_genes)) )
    {
      method <- names(object@metadata$marker_genes)[i]
      ## for each group
      for ( j in seq(length(object@metadata$marker_genes[[method]])) )
      {
        group <- names(object@metadata$marker_genes[[method]])[j]
        export$addMarkerGenes(
          method,
          group,
          object@metadata$marker_genes[[method]][[group]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## enriched pathways
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@metadata$enriched_pathways) )
  {
    ## check if it's a list
    if ( !is.list(object@metadata$enriched_pathways) )
    {
      stop(
        '`object@metadata$enriched_pathways` is not a list.',
        call. = FALSE
      )
    }
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Extracting pathway enrichment results...'
      )
    )
    ## for each method
    for ( i in seq(length(object@metadata$enriched_pathways)) )
    {
      method <- names(object@metadata$enriched_pathways)[i]
      ## for each group
      for ( j in seq(length(object@metadata$enriched_pathways[[method]])) )
      {
        group <- names(object@metadata$enriched_pathways[[method]])[j]
        export$addEnrichedPathways(
          method,
          group,
          object@metadata$enriched_pathways[[method]][[group]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## dimensional reductions
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'),
      '] Extracting dimensional reductions...'
    )
  )
  projections <- list()
  projections_available <- names(SingleCellExperiment::reducedDims(object))
  projections_available_pca <- projections_available[grep(
    projections_available, pattern = 'pca', ignore.case = TRUE, invert = FALSE
  )]
  projections_available_non_pca <- projections_available[grep(
    projections_available, pattern = 'pca', ignore.case = TRUE, invert = TRUE
  )]
  if ( length(projections_available) == 0 )
  {
    stop('No dimensional reductions available.', call. = FALSE)
  } else if (
    length(projections_available) == 1 &&
    length(projections_available_pca) == 1 )
  {
    SingleCellExperiment::reducedDims(export$expression)[[projections_available]] <- SingleCellExperiment::reducedDims(object)[[projections_available]]
    warning(
      paste0(
        'Warning: Only PCA as dimensional reduction found, will export ',
        'first and second principal components. Consider using tSNE and/or ',
        'UMAP instead.'
      )
    )
  } else if ( length(projections_available_non_pca) >= 1 )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] ',
        'Will export the following dimensional reductions: ',
        paste(projections_available_non_pca, collapse = ', ')
      )
    )
    for ( i in projections_available_non_pca )
    {
      SingleCellExperiment::reducedDims(export$expression)[[i]] <- SingleCellExperiment::reducedDims(object)[[i]]
    }
  }

  ##--------------------------------------------------------------------------##
  ## trajectories
  ##--------------------------------------------------------------------------##
  if ( length(object@metadata$trajectory) == 0 )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] No trajectories to extract...'
      )
    )
  } else
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] ',
        'Will export the following trajectories: ',
        paste(names(object@metadata$trajectories$monocle2), collapse = ', ')
      )
    )
    ## for each method
    for ( i in seq(length(object@metadata$trajectories)) )
    {
      method <- names(object@metadata$trajectories)[i]
      ## for each trajectory
      for ( j in seq(length(object@metadata$trajectories[[i]])) )
      {
        export$addTrajectory(
          method,
          names(object@metadata$trajectories[[i]])[j],
          object@metadata$trajectories[[i]][[j]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## group trees
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@metadata$trees) )
  {
    ## check if it's a list
    if ( !is.list(object@metadata$trees) )
    {
      stop(
        '`object@metadata$trees` is not a list.',
        call. = FALSE
      )
    }
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] Extracting trees...'
      )
    )
    for ( i in seq(length(object@metadata$trees)) )
    {
      export$addTree(
        names(object@metadata$trees)[i],
        object@metadata$trees[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## save export object to disk
  ##--------------------------------------------------------------------------##
  if ( !file.exists(dirname(file)) )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Creating output directory...'
      )
    )
    dir.create(dirname(file), showWarnings = FALSE)
  }
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Saving Cerebro file...'
    )
  )
  saveRDS(export, file)
}
