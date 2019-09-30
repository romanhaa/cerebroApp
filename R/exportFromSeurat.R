#' Export Seurat object to Cerebro.
#' @title Export Seurat object to Cerebro.
#' @description This function allows to export a Seurat object to visualize in
#' Cerebro.
#' @keywords Cerebro scRNAseq Seurat
#' @param object Seurat object.
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
#' @examples
#' exportFromSeurat(
#'   object = seurat,
#'   file = 'PDX_patient_A.crb',
#'   experiment_name = 'PDX_patient_A',
#'   organism = 'hg',
#'   column_sample = 'sample',
#'   column_cluster = 'cluster',
#'   column_nUMI = 'nUMI',
#'   column_nGene = 'nGene',
#'   column_cell_cycle_seurat = 'cell_cycle_seurat',
#'   column_cell_cycle_cyclone = 'cell_cycle_cyclone',
#'   add_all_meta_data = TRUE
#' )
exportFromSeurat <- function(
  object,
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
  # check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Package 'Seurat' needed for this function to work. Please install it.", call. = FALSE)
  }
  ##--------------------------------------------------------------------------##
  ## check provided parameters
  ##--------------------------------------------------------------------------##
  if ( (column_sample %in% names(object@meta.data) == FALSE ) ) {
    stop(
      'Column specified in `column_sample` not found in meta data.',
      call. = FALSE
    )
  }
  if ( (column_cluster %in% names(object@meta.data) == FALSE ) ) {
    stop(
      'Column specified in `column_cluster` not found in meta data.',
      call. = FALSE
    )
  }
  if ( (column_nUMI %in% names(object@meta.data) == FALSE ) ) {
    stop(
      'Column with number of transcripts per cell (`nUMI`) not found in meta data.',
      call. = FALSE
    )
  }
  if ( (column_nGene %in% names(object@meta.data) == FALSE ) ) {
    stop(
      'Column with number of expressed genes per cell (`nGene`) not found in meta data.',
      call. = FALSE
    )
  }
  ##--------------------------------------------------------------------------##
  ## colors
  ##--------------------------------------------------------------------------##
  # Dutch palette from flatuicolors.com
  colors_dutch <- c(
    '#FFC312','#C4E538','#12CBC4','#FDA7DF','#ED4C67',
    '#F79F1F','#A3CB38','#1289A7','#D980FA','#B53471',
    '#EE5A24','#009432','#0652DD','#9980FA','#833471',
    '#EA2027','#006266','#1B1464','#5758BB','#6F1E51'
  )
  # Spanish palette from flatuicolors.com
  colors_spanish <- c(
    '#40407a','#706fd3','#f7f1e3','#34ace0','#33d9b2',
    '#2c2c54','#474787','#aaa69d','#227093','#218c74',
    '#ff5252','#ff793f','#d1ccc0','#ffb142','#ffda79',
    '#b33939','#cd6133','#84817a','#cc8e35','#ccae62'
  )
  colors <- c(colors_dutch, colors_spanish)
  cell_cycle_colorset <- setNames(
    c('#45aaf2', '#f1c40f', '#e74c3c', '#7f8c8d'),
    c('G1',      'S',       'G2M',     '-')
  )
  ##--------------------------------------------------------------------------##
  ## initialize export object
  ##--------------------------------------------------------------------------##
  message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Initializing Cerebro object...'))
  export <- list(
    experiment = list(
      experiment_name = experiment_name,
      organism = organism
    )
  )
  ##--------------------------------------------------------------------------##
  ## collect some more data if present
  ##--------------------------------------------------------------------------##
  #
  export$experiment$date_of_analysis <- object@misc$experiment$date_of_analysis
  #
  if ( is.null(object@misc$parameters) ) {
    export$parameters <- list()
  } else {
    export$parameters <- object@misc$parameters
  }
  #
  if ( is.null(object@misc$gene_lists) ) {
    export$gene_lists <- list()
  } else {
    export$gene_lists <- object@misc$gene_lists
  }
  #
  if ( is.null(object@misc$technical_info) ) {
    export$technical_info <- list()
  } else {
    export$technical_info <- object@misc$technical_info
  }
  if ( is.null(object@misc$technical_info$seurat_version) ) {
    export$technical_info$seurat_version <- object@version
  }
  ##--------------------------------------------------------------------------##
  ## samples
  ##--------------------------------------------------------------------------##
  if ( is.factor(object@meta.data[[column_sample]]) ) {
    sample_names <- levels(object@meta.data[[column_sample]])
  } else {
    sample_names <- unique(object@meta.data[[column_sample]])
  }
  export$samples <- list(
    colors = setNames(colors[ 1:length(sample_names) ], sample_names),
    overview = data.frame('sample' = sample_names)
  )
  ##--------------------------------------------------------------------------##
  ## clusters
  ##--------------------------------------------------------------------------##
  if ( is.factor(object@meta.data[[column_cluster]]) ) {
    cluster_names <- levels(object@meta.data[[column_cluster]])
  } else {
    cluster_names <- sort(unique(object@meta.data[[column_cluster]]))
  }
  export$clusters <- list(
    colors = setNames(colors[ 1:length(cluster_names) ], cluster_names),
    overview = data.frame('cluster' = cluster_names)
  )
  ##--------------------------------------------------------------------------##
  ## meta data
  ##--------------------------------------------------------------------------##
  message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Collecting available meta data...'))
  meta_data_columns <- names(object@meta.data)
  export$cells <- data.frame(
    'sample' = factor(object@meta.data[[column_sample]], levels = c(sample_names)),
    'cluster' = factor(object@meta.data[[column_cluster]], levels = c(cluster_names)),
    'nUMI' = object@meta.data[[column_nUMI]],
    'nGene' = object@meta.data[[column_nGene]]
  )
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_sample)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_cluster)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_nUMI)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_nGene)]
  ##--------------------------------------------------------------------------##
  ## samples by cluster
  ##--------------------------------------------------------------------------##
  message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Stratifying samples by clusters...'))
  export$samples$by_cluster <- export$cells %>%
    dplyr::group_by(sample, cluster) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    tidyr::spread(cluster, count, fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
    dplyr::select(c('sample', 'total_cell_count', dplyr::everything())) %>%
    dplyr::arrange(factor(sample, levels = sample_names))
  ##--------------------------------------------------------------------------##
  ## clusters by sample
  ##--------------------------------------------------------------------------##
  message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Stratifying clusters by samples...'))
  export$clusters$by_samples <- export$cells %>%
    dplyr::group_by(cluster, sample) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    tidyr::spread(sample, count, fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
    dplyr::select(c('cluster', 'total_cell_count', dplyr::everything())) %>%
    dplyr::arrange(factor(cluster, levels = cluster_names))
  ##--------------------------------------------------------------------------##
  ## cell cycle Seurat (if present)
  ##--------------------------------------------------------------------------##
  if ( !is.null(column_cell_cycle_seurat) && column_cell_cycle_seurat %in% names(object@meta.data) ) {
    message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Processing cell cycle data generated by Seurat...'))
    export$cells$cell_cycle_seurat <- object@meta.data[[column_cell_cycle_seurat]]
    # by sample
    export$samples$by_cell_cycle_seurat <- export$cells %>%
      dplyr::group_by(sample, cell_cycle_seurat) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      tidyr::spread(cell_cycle_seurat, count, fill = 0) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
      dplyr::select(c('sample', 'total_cell_count', dplyr::everything())) %>%
      dplyr::arrange(factor(sample, levels = sample_names))
    # by cluster
    export$clusters$by_cell_cycle_seurat <- export$cells %>%
      dplyr::group_by(cluster, cell_cycle_seurat) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      tidyr::spread(cell_cycle_seurat, count, fill = 0) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
      dplyr::select(c('cluster', 'total_cell_count', dplyr::everything())) %>%
      dplyr::arrange(factor(cluster, levels = cluster_names))
    meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_cell_cycle_seurat)]
  }
  ##--------------------------------------------------------------------------##
  ## cell cycle Cyclone (if present)
  ##--------------------------------------------------------------------------##
  if ( !is.null(column_cell_cycle_cyclone) && column_cell_cycle_cyclone %in% names(object@meta.data) ) {
    message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Processing cell cycle data generated by Cyclone...'))
    export$cells$cell_cycle_cyclone <- object@meta.data[[column_cell_cycle_cyclone]]
    # by sample
    export$samples$by_cell_cycle_cyclone <- export$cells %>%
      dplyr::group_by(sample, cell_cycle_cyclone) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      tidyr::spread(cell_cycle_cyclone, count, fill = 0) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
      dplyr::select(c('sample', 'total_cell_count', dplyr::everything())) %>%
      dplyr::arrange(factor(sample, levels = sample_names))
    # by cluster
    export$clusters$by_cell_cycle_cyclone <- export$cells %>%
      dplyr::group_by(cluster, cell_cycle_cyclone) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      tidyr::spread(cell_cycle_cyclone, count, fill = 0) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
      dplyr::select(c('cluster', 'total_cell_count', dplyr::everything())) %>%
      dplyr::arrange(factor(cluster, levels = cluster_names))
    meta_data_columns <- meta_data_columns[-which(meta_data_columns == column_cell_cycle_cyclone)]
  }
  ##--------------------------------------------------------------------------##
  ## cell barcode
  ##--------------------------------------------------------------------------##
  if ( !is.null(rownames(as.data.frame(object@meta.data))) ) {
    export$cells$cell_barcode <- rownames(as.data.frame(object@meta.data))
  }
  ##--------------------------------------------------------------------------##
  ## add all other meta data if specified
  ##--------------------------------------------------------------------------##
  if ( add_all_meta_data ) {
    message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Extracting all meta data columns...'))
    export$cells <- cbind(export$cells, object@meta.data[meta_data_columns])
  }
  ##--------------------------------------------------------------------------##
  ## most expressed genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$most_expressed_genes) ) {
    message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Extracting tables of most expressed genes...'))
    export$most_expressed_genes <- object@misc$most_expressed_genes
  }
  ##--------------------------------------------------------------------------##
  ## marker genes
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$marker_genes) ) {
    message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Extracting tables of marker genes...'))
    export$marker_genes <- object@misc$marker_genes
  }
  ##--------------------------------------------------------------------------##
  ## enriched pathways
  ##--------------------------------------------------------------------------##
  if ( !is.null(object@misc$enriched_pathways) ) {
    message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Extracting pathway enrichment results...'))
    export$enriched_pathways <- object@misc$enriched_pathways
  }
  ##--------------------------------------------------------------------------##
  ## dimensional reductions
  ##--------------------------------------------------------------------------##
  message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Extracting dimensional reductions...'))
  export$projections <- list()
  if ( object@version < 3 ) {
    projections_available <- names(object@dr)
    projections_available_pca <- projections_available[grep(projections_available, pattern = 'pca', ignore.case = TRUE, invert = FALSE)]
    projections_available_non_pca <- projections_available[grep(projections_available, pattern = 'pca', ignore.case = TRUE, invert = TRUE)]
    if ( length(projections_available) == 0 ) {
      stop('Warning: No dimensional reductions available.', call. = FALSE)
    } else if ( length(projections_available) == 1 && length(projections_available_pca) == 1 ) {
      warning('Warning: Only PCA as dimensional reduction found, will export first and second principal components. Consider using tSNE and/or UMAP instead.')
      export$projections[[projections_available]] <- as.data.frame(object@dr[[projections_available]]@cell.embeddings)
    } else if ( length(projections_available_non_pca) > 0 ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] ',
          'Will export the following dimensional reductions: ',
          paste(projections_available_non_pca, collapse = ', ')
        )
      )
      for ( i in projections_available_non_pca ) {
        export$projections[[i]] <- as.data.frame(object@dr[[i]]@cell.embeddings)
      }
    }
  } else {
    projections_available <- names(object@reductions)
    projections_available_pca <- projections_available[grep(projections_available, pattern = 'pca', ignore.case = TRUE, invert = FALSE)]
    projections_available_non_pca <- projections_available[grep(projections_available, pattern = 'pca', ignore.case = TRUE, invert = TRUE)]
    if ( length(projections_available) == 0 ) {
      stop('Warning: No dimensional reductions available.', call. = FALSE)
    } else if ( length(projections_available) == 1 && length(projections_available_pca) == 1 ) {
      export$projections[[projections_available]] <- as.data.frame(object@reductions[[projections_available]]@cell.embeddings)
      warning('Warning: Only PCA as dimensional reduction found, will export first and second principal components. Consider using tSNE and/or UMAP instead.')
    } else if ( length(projections_available_non_pca) >= 1 ) {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'), '] ',
          'Will export the following dimensional reductions: ',
          paste(projections_available_non_pca, collapse = ', ')
        )
      )
      for ( i in projections_available_non_pca ) {
        export$projections[[i]] <- as.data.frame(object@reductions[[i]]@cell.embeddings)
      }
    }
  }
  ##--------------------------------------------------------------------------##
  ## trajectories
  ##--------------------------------------------------------------------------##
  if ( length(seurat@misc$trajectory) == 0 ) {
    message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] No trajectories to extracting...'))
  } else {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] ',
        'Will export the following trajectories: ',
        paste(names(seurat@misc$trajectory$monocle2), collapse = ', ')
      )
    )
    export$trajectory <- seurat@misc$trajectory
  }
  ##--------------------------------------------------------------------------##
  ## cluster tree
  ##--------------------------------------------------------------------------##
  if ( object@version < 3 ) {
    if ( !is.null(object@cluster.tree) ) {
      message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Extracting cluster tree...'))
      export$clusters$tree <- object@cluster.tree[[1]]
    }
  } else {
    if ( !is.null(object@tools$BuildClusterTree) ) {
      message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Extracting cluster tree...'))
      export$clusters$tree <- object@tools$BuildClusterTree
    }
  }
  ##--------------------------------------------------------------------------##
  ## log-normalized expression
  ##--------------------------------------------------------------------------##
  message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Extracting log-normalized expression data...'))
  if ( object@version < 3 ) {
    export$expression <- object@data
  } else {
    export$expression <- object@assays$RNA@data
  }
  ##--------------------------------------------------------------------------##
  ## save export object to disk
  ##--------------------------------------------------------------------------##
  if ( !file.exists(dirname(file)) ) {
    message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Creating output directory...'))
    dir.create(dirname(file), showWarnings = FALSE)
  }
  message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Saving Cerebro file...'))
  saveRDS(export, file)
}