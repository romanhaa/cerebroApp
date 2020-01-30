#' Export Seurat object to Cerebro.
#' @title Export Seurat object to Cerebro.
#' @description This function allows to export a Seurat object to visualize in
#' Cerebro.
#' @keywords Cerebro scRNAseq Seurat
#' @param object Seurat object.
#' @param assay Assay to pull counts from; defaults to 'RNA'. Only relevant in
#' Seurat v3.0 or higher since the concept of assays wasn't implemented before.
#' @param file Where to save the output.
#' @param experiment_name Experiment name.
#' @param organism Organism, e.g. hg (human), mm (mouse), etc.
#' @param column_sample Column in object@meta.data that contains information
#' about sample; defaults to 'sample'.
#' @param column_cluster Column in object@meta.data that contains information
#' about cluster; defaults to 'cluster'.
#' @param column_nUMI Column in object@meta.data that contains information about
#' number of transcripts per cell; defaults to 'nUMI'.
#' @param column_nGene Column in object@meta.data that contains information
#' about number of expressed genes per cell; defaults to 'nGene'.
#' @param column_cell_cycle_seurat Optional column in object@meta.data that
#' contains information about cell cycle phase based on Regev method (default of
#' Seurat); defaults to NULL.
#' @param column_cell_cycle_cyclone Optional column in object@meta.data that
#' contains information about cell cycle phase based on Cyclone method; defaults
#' to NULL.
#' @param add_all_meta_data If set to TRUE, all further meta data columns will
#' be extracted as well.
#' @export
#' @return Nothing.
#' @import dplyr
#' @importFrom rlang .data
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.2/seurat_pbmc.rds",
#'   package = "cerebroApp"))
#' exportFromSeurat(
#'   object = pbmc,
#'   file = 'pbmc.crb',
#'   experiment_name = 'PBMC',
#'   organism = 'hg',
#'   column_sample = 'sample',
#'   column_cluster = 'seurat_clusters',
#'   column_nUMI = 'nCount_RNA',
#'   column_nGene = 'nFeature_RNA'
#' )
exportFromSeurat <- function(
  object,
  assay = 'RNA',
  file,
  experiment_name,
  organism,
  column_sample = 'sample',
  column_cluster = 'cluster',
  column_nUMI = 'nUMI',
  column_nGene = 'nGene',
  column_cell_cycle_seurat = NULL,
  column_cell_cycle_cyclone = NULL,
  add_all_meta_data = TRUE
) {
  ## check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE))
  {
    stop(
      "Package 'Seurat' needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  ##--------------------------------------------------------------------------##
  ## check provided parameters
  ##--------------------------------------------------------------------------##
  ## `column_sample`
  if ( (column_sample %in% names(object@meta.data) == FALSE ) )
  {
    stop(
      'Column specified in `column_sample` not found in meta data.',
      call. = FALSE
    )
  }
  ## `column_cluster`
  if ( (column_cluster %in% names(object@meta.data) == FALSE ) )
  {
    stop(
      'Column specified in `column_cluster` not found in meta data.',
      call. = FALSE
    )
  }
  ## `column_nUMI`
  if ( (column_nUMI %in% names(object@meta.data) == FALSE ) )
  {
    stop(
      paste0(
        'Column with number of transcripts per cell (`nUMI`) not found in ',
        'meta data.'
      ),
      call. = FALSE
    )
  }
  ## `column_nGene`
  if ( (column_nGene %in% names(object@meta.data) == FALSE ) )
  {
    stop(
      paste0(
        'Column with number of expressed genes per cell (`nGene`) not found ',
        'in meta data.'
      ),
      call. = FALSE
    )
  }
  ##--------------------------------------------------------------------------##
  ## initialize export object
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Initializing Cerebro object...'
    )
  )
  export <- list(
    experiment = list(
      experiment_name = experiment_name,
      organism = organism
    )
  )
  ##--------------------------------------------------------------------------##
  ## collect some more data if present
  ##--------------------------------------------------------------------------##
  ## data of analysis
  export$experiment$date_of_analysis <- object@misc$experiment$date_of_analysis
  ## data of export
  export$experiment$date_of_export <- Sys.Date()
  ## `parameters`
  if ( is.null(object@misc$parameters) )
  {
    export$parameters <- list()
  } else
  {
    export$parameters <- object@misc$parameters
  }
  ## `gene_lists`
  if ( is.null(object@misc$gene_lists) )
  {
    export$gene_lists <- list()
  } else
  {
    export$gene_lists <- object@misc$gene_lists
  }
  ## technical_info`
  if ( is.null(object@misc$technical_info) )
  {
    export$technical_info <- list()
  } else
  {
    export$technical_info <- object@misc$technical_info
  }
  ## Seurat version
  if ( !is.null(object@version) )
  {
    export$technical_info$seurat_version <- object@version
  }
  ## cerebroApp version
  export$technical_info$cerebroApp_version <- utils::packageVersion('cerebroApp')
  ##--------------------------------------------------------------------------##
  ## get sample names
  ##--------------------------------------------------------------------------##
  if ( is.factor(object@meta.data[[column_sample]]) )
  {
    sample_names <- levels(object@meta.data[[column_sample]])
  } else
  {
    sample_names <- unique(object@meta.data[[column_sample]])
  }
  ##--------------------------------------------------------------------------##
  ## get cluster names
  ##--------------------------------------------------------------------------##
  if ( is.factor(object@meta.data[[column_cluster]]) )
  {
    cluster_names <- levels(object@meta.data[[column_cluster]])
  } else
  {
    cluster_names <- unique(object@meta.data[[column_cluster]]) %>% sort()
  }
  ##--------------------------------------------------------------------------##
  ## meta data
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Collecting available meta data...'
    )
  )
  meta_data_columns <- names(object@meta.data)
  export$cells <- data.frame(
    'sample' = factor(
      object@meta.data[[column_sample]], levels = c(sample_names)
    ),
    'cluster' = factor(
      object@meta.data[[column_cluster]], levels = c(cluster_names)
    ),
    'nUMI' = object@meta.data[[column_nUMI]],
    'nGene' = object@meta.data[[column_nGene]]
  )
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_sample)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_cluster)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_nUMI)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_nGene)]
  ##--------------------------------------------------------------------------##
  ## cell barcode
  ##--------------------------------------------------------------------------##
  if ( !is.null(rownames(as.data.frame(object@meta.data))) )
  {
    export$cells$cell_barcode <- rownames(as.data.frame(object@meta.data))
  }
  ##--------------------------------------------------------------------------##
  ## add all other meta data if specified
  ##--------------------------------------------------------------------------##
  if ( add_all_meta_data )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Extracting all meta data columns...'
      )
    )
    export$cells <- cbind(export$cells, object@meta.data[meta_data_columns])
  }
  ##--------------------------------------------------------------------------##
  ## most expressed genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$most_expressed_genes) )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Extracting tables of most expressed genes...'
      )
    )
    export$most_expressed_genes <- object@misc$most_expressed_genes
  }
  ##--------------------------------------------------------------------------##
  ## marker genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$marker_genes) )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Extracting tables of marker genes...'
      )
    )
    export$marker_genes <- object@misc$marker_genes
  }
  ##--------------------------------------------------------------------------##
  ## enriched pathways
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$enriched_pathways) )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Extracting pathway enrichment results...'
      )
    )
    export$enriched_pathways <- object@misc$enriched_pathways
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
  export$projections <- list()
  if ( object@version < 3 )
  {
    projections_available <- names(object@dr)
    projections_available_pca <- projections_available[grep(
      projections_available, pattern = 'pca', ignore.case = TRUE, invert = FALSE
    )]
    projections_available_non_pca <- projections_available[grep(
      projections_available, pattern = 'pca', ignore.case = TRUE, invert = TRUE
    )]
    if ( length(projections_available) == 0 )
    {
      stop('Warning: No dimensional reductions available.', call. = FALSE)
    } else if (
      length(projections_available) == 1 &&
      length(projections_available_pca) == 1 )
    {
      warning(
        paste0(
          'Warning: Only PCA as dimensional reduction found, will export ',
          'first and second principal components. Consider using tSNE and/or ',
          'UMAP instead.'
        )
      )
      export$projections[[projections_available]] <- as.data.frame(
        object@dr[[projections_available]]@cell.embeddings
      )
    } else if ( length(projections_available_non_pca) > 0 )
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
        export$projections[[i]] <- as.data.frame(object@dr[[i]]@cell.embeddings)
      }
    }
  } else
  {
    projections_available <- names(object@reductions)
    projections_available_pca <- projections_available[grep(
      projections_available, pattern = 'pca', ignore.case = TRUE, invert = FALSE
    )]
    projections_available_non_pca <- projections_available[grep(
      projections_available, pattern = 'pca', ignore.case = TRUE, invert = TRUE
    )]
    if ( length(projections_available) == 0 )
    {
      stop('Warning: No dimensional reductions available.', call. = FALSE)
    } else if (
      length(projections_available) == 1 &&
      length(projections_available_pca) == 1 )
    {
      export$projections[[projections_available]] <- as.data.frame(
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
        export$projections[[i]] <- as.data.frame(
          object@reductions[[i]]@cell.embeddings
        )
      }
    }
  }
  ##--------------------------------------------------------------------------##
  ## trajectories
  ##--------------------------------------------------------------------------##
  if ( length(object@misc$trajectory) == 0 )
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
        paste(names(object@misc$trajectory$monocle2), collapse = ', ')
      )
    )
    export$trajectory <- object@misc$trajectory
  }
  ##--------------------------------------------------------------------------##
  ## cluster tree
  ##--------------------------------------------------------------------------##
  if ( object@version < 3 )
  {
    if ( !is.null(object@cluster.tree) )
    {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] Extracting cluster tree...'
        )
      )
      export$clusters$tree <- object@cluster.tree[[1]]
    }
  } else
  {
    if ( !is.null(object@tools$BuildClusterTree) )
    {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] Extracting cluster tree...'
        )
      )
      export$clusters$tree <- object@tools$BuildClusterTree
    }
  }
  ##--------------------------------------------------------------------------##
  ## log-normalized expression
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'),
      '] Extracting log-normalized expression data...'
    )
  )
  if ( object@version < 3 )
  {
    ## check if `data` matrix exist in provided Seurat object
    if ( ('data' %in% names(object) == FALSE ) )
    {
      stop(
        paste0(
          '`data` matrix could not be found in provided Seurat ',
          'object.'
        ),
        call. = FALSE
      )
    }
    export$expression <- object@data
  } else
  {
    ## check if provided assay exists
    if ( (assay %in% names(object@assays) == FALSE ) )
    {
      stop(
        paste0(
          'Assay slot `', assay, '` could not be found in provided Seurat ',
          'object.'
        ),
        call. = FALSE
      )
    }
    ## check if `data` matrix exist in provided assay
    if ( is.null(object@assays[[assay]]@data) )
    {
      stop(
        paste0(
          '`data` matrix could not be found in `', assay, '` assay slot.'
        ),
        call. = FALSE
      )
    }
    export$expression <- object@assays[[assay]]@data
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
