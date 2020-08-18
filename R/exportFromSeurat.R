#' Export Seurat object to Cerebro.
#' @title Export Seurat object to Cerebro.
#' @description This function allows to export a Seurat object to visualize in
#' Cerebro.
#' @keywords Cerebro scRNAseq Seurat
#' @param object Seurat object.
#' @param assay Assay to pull expression values from; defaults to 'RNA'.
#' @param slot Slot to pull expression values from; defaults to 'data'. It is
#' recommended to use sparse data (such as log-transformed or raw counts)
#' instead of dense data (such as the 'scaled' slot) to avoid performance
#' bottlenecks in the Cerebro interface.
#' @param file Where to save the output.
#' @param experiment_name Experiment name.
#' @param organism Organism, e.g. hg (human), mm (mouse), etc.
#' @param columns_groups Names of grouping variables in meta data
#' (object@meta.data), e.g. c("sample","cluster"); at least one must be
#' provided; defaults to NULL.
#' @param column_nUMI Column in object@meta.data that contains information about
#' number of transcripts per cell; defaults to 'nUMI'.
#' @param column_nGene Column in object@meta.data that contains information
#' about number of expressed genes per cell; defaults to 'nGene'.
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
#' pbmc <- readRDS(system.file("extdata/v1.2/seurat_pbmc.rds",
#'   package = "cerebroApp"))
#' exportFromSeurat(
#'   object = pbmc,
#'   file = 'pbmc_Seurat.crb',
#'   experiment_name = 'PBMC',
#'   organism = 'hg',
#'   columns_groups = c('sample','cluster'),
#'   column_nUMI = 'nCount_RNA',
#'   column_nGene = 'nFeature_RNA',
#'   columns_cell_cycle = c('Phase')
#' )
exportFromSeurat <- function(
  object,
  assay = 'RNA',
  slot = 'data',
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
  ## check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE))
  {
    stop(
      paste0(
        "Package 'Seurat' is needed for this function to work. ",
        "Please install it."
      ),
      call. = FALSE
    )
  }
  ## check if provided object is a Seurat object
  if ( tolower(class(object)) != 'seurat' )
  {
    stop(
      paste0(
        "The provided object (class `", class(object), "`) does not look ",
        "like a Seurat object."
      ),
      call. = FALSE
    )
  }
  ## check if Seurat object is too old
  if ( object@version < 3 )
  {
    stop(
      paste0(
        "Objects created by Seurat versions prior to v3.0 are no longer ",
        "supported. Please use the `UpdateSeuratObject()` function or use an ",
        "older version of Cerebro."
      ),
      call. = FALSE
    )
  }
  ## `columns_groups`
  if ( any(columns_groups %in% names(object@meta.data) == FALSE ) )
  {
    stop(
      paste0(
        'Some group columns could not be found in meta data: ',
        paste0(
          columns_groups[which(columns_groups %in% names(object@meta.data) == FALSE)],
          collapse = ', '
        )
      ),
      call. = FALSE
    )
  }
  ## `column_nUMI`
  if ( ( column_nUMI %in% names(object@meta.data) == FALSE ) )
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
  if ( (column_nGene %in% names(object@meta.data) == FALSE ) )
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
  if ( any(columns_cell_cycle %in% names(object@meta.data) == FALSE ) )
  {
    stop(
      paste0(
        'Some cell cycle columns could not be found in meta data: ',
        paste0(
          columns_cell_cycle[which(columns_cell_cycle %in% names(object@meta.data) == FALSE)],
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
  if ( (assay %in% names(object@assays) == FALSE ) )
  {
    stop(
      paste0(
        'Specified assay `', assay, '` could not be found in provided Seurat ',
        'object.'
      ),
      call. = FALSE
    )
  }

  expression_data <- try(
    Seurat::GetAssayData(object, assay = assay, slot = slot),
    silent = TRUE
  )

  ## convert expression data to "RleArray" if it is "dgCMatrix"
  if ( class(expression_data) == 'dgCMatrix' )
  {
    expression_data <- as(expression_data, "RleArray")
  }

  ## check if provided slot exists in provided assay
  if ( class(expression_data) == 'try-error' ) {
    stop(
      paste0(
        'Slot `', slot, '` could not be found in `', assay, '` assay slot.'
      ),
      call. = FALSE
    )
  }
  ## create new Cerebro object
  export <- Cerebro_v1.3$new()

  ## add experiment name
  export$addExperiment('experiment_name', 'pbmc_Seurat')

  ## add organism
  export$addExperiment('organism', 'hg')

  ## add cerebroApp version
  export$setVersion(utils::packageVersion('cerebroApp'))

  ## add expression data
  export$expression <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = expression_data)
  )

  ##--------------------------------------------------------------------------##
  ## collect some more data if present
  ##--------------------------------------------------------------------------##
  ## data of analysis
  export$addExperiment('date_of_analysis', object@misc$experiment$date_of_analysis)

  ## date of export
  export$addExperiment('date_of_export', Sys.Date())

  ## `parameters`
  if ( !is.null(object@misc$parameters) )
  {
    for ( i in seq(length(object@misc$parameters)) )
    {
      name <- names(object@misc$parameters)[i]
      export$addParameters(
        name,
        object@misc$parameters[[name]]
      )
    }
  }

  ## `technical_info`
  if ( !is.null(object@misc$technical_info) )
  {
    for ( i in seq(length(object@misc$technical_info)) )
    {
      export$addTechnicalInfo(
        names(object@misc$technical_info)[i],
        object@misc$technical_info[[i]]
      )
    }
  }

  ## `gene_lists`
  if ( !is.null(object@misc$gene_lists) )
  {
    for ( i in seq(length(object@misc$gene_lists)) )
    {
      export$addGeneList(
        names(object@misc$gene_lists)[i],
        object@misc$gene_lists[[i]]
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
  colData(export$expression)$cell_barcode <- Cells(object)

  ## factorize group variables and add groups
  for ( i in columns_groups )
  {
    if ( is.factor(object@meta.data[[i]]) )
    {
      tmp_names <- levels(object@meta.data[[i]])
    } else
    {
      tmp_names <- unique(object@meta.data[[i]])
    }
    colData(export$expression)[[i]] <- factor(object@meta.data[[i]], levels = tmp_names)
    export$addGroup(i, tmp_names)
  }

  ## number of transcripts and expressed genes
  colData(export$expression)$nUMI = object@meta.data[[column_nUMI]]
  colData(export$expression)$nGene = object@meta.data[[column_nGene]]

  ## rest of meta data
  meta_data_columns <- names(object@meta.data)
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
      if ( is.factor(object@meta.data[[i]]) )
      {
        tmp_names <- levels(object@meta.data[[i]])
      } else
      {
        tmp_names <- unique(object@meta.data[[i]])
      }
      colData(export$expression)[[i]] <- factor(object@meta.data[[i]], levels = tmp_names)
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
      colData(export$expression)[[i]] <- object@meta.data[[i]]
    }
  }

  ## make column names in meta data unique (if necessary)
  colnames(colData(export$expression)) <- make.unique(colnames(colData(export$expression)))

  ##--------------------------------------------------------------------------##
  ## most expressed genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$most_expressed_genes) )
  {
    ## check if it's a list
    if ( !is.list(object@misc$most_expressed_genes) )
    {
      stop(
        '`object@misc$most_expressed_genes` is not a list.',
        call. = FALSE
      )
    }
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Extracting tables of most expressed genes...'
      )
    )
    for ( i in seq(length(object@misc$most_expressed_genes)) )
    {
      export$addMostExpressedGenes(
        names(object@misc$most_expressed_genes)[i],
        object@misc$most_expressed_genes[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## marker genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$marker_genes) )
  {
    ## check if it's a list
    if ( !is.list(object@misc$marker_genes) )
    {
      stop(
        '`object@misc$marker_genes` is not a list.',
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
    for ( i in seq(length(object@misc$marker_genes)) )
    {
      method <- names(object@misc$marker_genes)[i]
      ## for each group
      for ( j in seq(length(object@misc$marker_genes[[method]])) )
      {
        group <- names(object@misc$marker_genes[[method]])[j]
        export$addMarkerGenes(
          method,
          group,
          object@misc$marker_genes[[method]][[group]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## enriched pathways
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$enriched_pathways) )
  {
    ## check if it's a list
    if ( !is.list(object@misc$enriched_pathways) )
    {
      stop(
        '`object@misc$enriched_pathways` is not a list.',
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
    for ( i in seq(length(object@misc$enriched_pathways)) )
    {
      method <- names(object@misc$enriched_pathways)[i]
      ## for each group
      for ( j in seq(length(object@misc$enriched_pathways[[method]])) )
      {
        group <- names(object@misc$enriched_pathways[[method]])[j]
        export$addEnrichedPathways(
          method,
          group,
          object@misc$enriched_pathways[[method]][[group]]
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
  projections_available <- names(object@reductions)
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
    SingleCellExperiment::reducedDims(export$expression)[[projections_available]] <- as.data.frame(
      object@reductions[[projections_available]]@cell.embeddings
    )
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
      SingleCellExperiment::reducedDims(export$expression)[[i]] <- as.data.frame(
        object@reductions[[i]]@cell.embeddings
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## trajectories
  ##--------------------------------------------------------------------------##
  if ( length(object@misc$trajectories) == 0 )
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
        paste(names(object@misc$trajectories$monocle2), collapse = ', ')
      )
    )
    ## for each method
    for ( i in seq(length(object@misc$trajectories)) )
    {
      method <- names(object@misc$trajectories)[i]
      ## for each trajectory
      for ( j in seq(length(object@misc$trajectories[[i]])) )
      {
        export$addTrajectory(
          method,
          names(object@misc$trajectories[[i]])[j],
          object@misc$trajectories[[i]][[j]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## group trees
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$trees) )
  {
    ## check if it's a list
    if ( !is.list(object@misc$trees) )
    {
      stop(
        '`object@misc$trees` is not a list.',
        call. = FALSE
      )
    }
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] Extracting trees...'
      )
    )
    for ( i in seq(length(object@misc$trees)) )
    {
      export$addTree(
        names(object@misc$trees)[i],
        object@misc$trees[[i]]
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
