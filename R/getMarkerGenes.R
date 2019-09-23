#' Get marker genes for every sample and cluster in Seurat object.
#' @title Get marker genes for every sample and cluster in Seurat object.
#' @description This function gets marker genes for every sample and cluster of
#' the Seurat object.
#' @param object Seurat object.
#' @param organism Organism information for pulling info about presence of
#' marker genes of cell surface; can be omitted if already saved in Seurat
#' object; defaults to NULL.
#' @param column_sample Column in object@meta.data that contains information
#' about sample; defaults to 'sample'.
#' @param column_cluster Column in object@meta.data that contains information
#' about cluster; defaults to 'cluster'.
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
#' @keywords seurat cerebro
#' @export
#' @import dplyr
#' @import Seurat
#' @examples
#' seurat <- getMarkerGenes(
#'   object = seurat
#'   organism = 'hg',
#'   column_sample = 'sample',
#'   column_cluster = 'cluster',
#'   only_pos = TRUE,
#'   min_pct = 0.7,
#'   thresh_logFC = 0.25,
#'   thresh_p_val = 0.01,
#'   test = 'wilcox',
#'   verbose = TRUE
#' )
getMarkerGenes <- function(
  object,
  organism = NULL,
  column_sample = 'sample',
  column_cluster = 'cluster',
  only_pos = TRUE,
  min_pct = 0.70,
  thresh_logFC = 0.25,
  thresh_p_val = 0.01,
  test = 'wilcox',
  verbose = TRUE,
  ...
) {
  ##--------------------------------------------------------------------------##
  ## Get list of genes in cell surface through gene ontology term GO:0009986.
  ##--------------------------------------------------------------------------##
  if ( organism == 'hg' || organism == 'human' ) {
    genes_on_cell_surface <- biomaRt::getBM(
      attributes = 'hgnc_symbol',
      filters = 'go',
      values = 'GO:0009986',
      mart = biomaRt::useMart('ensembl', dataset = 'hsapiens_gene_ensembl')
    )[,1]
  } else if ( organism == 'mm' || organism == 'mouse' ) {
    genes_on_cell_surface <- biomaRt::getBM(
      attributes = 'external_gene_name',
      filters = 'go',
      values = 'GO:0009986',
      mart = biomaRt::useMart('ensembl', dataset = 'mmusculus_gene_ensembl')
    )[,1]
  } else {
    message('No information about genes on cell surface because organism is either not specified or not human/mouse.')
  }
  ##--------------------------------------------------------------------------##
  ## make copy of Seurat object
  ##--------------------------------------------------------------------------##
  temp_seurat <- object
  ##--------------------------------------------------------------------------##
  ## create slot for results in Seurat object
  ##--------------------------------------------------------------------------##
  if ( is.null(object@misc$marker_genes) ) {
    temp_seurat@misc$marker_genes <- list()
  }
  temp_seurat@misc$marker_genes$parameters <- list(
    only_positive = only_pos,
    minimum_percentage = min_pct,
    logFC_threshold = thresh_logFC,
    p_value_threshold = thresh_p_val,
    test = test
  )
  ##--------------------------------------------------------------------------##
  ## samples
  ## - check if column_sample is provided and exists in meta data
  ## - get sample names
  ## - check if more than 1 sample exists
  ## - run Seurat::FindAllMarkers()
  ## - check if any marker genes were found
  ## - sort and rename columns
  ## - add column with cell surface genes if present
  ## - store results in Seurat object
  ##--------------------------------------------------------------------------##
  #
  if ( !is.null(column_sample) && (column_sample %in% names(temp_seurat@meta.data)) ) {
    #
    if ( is.factor(temp_seurat@meta.data[[column_sample]]) ) {
      sample_names <- levels(temp_seurat@meta.data[[column_sample]])
    } else {
      sample_names <- unique(temp_seurat@meta.data[[column_sample]])
    }
    #
    if ( length(sample_names) > 1 ) {
      message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Get marker genes for samples...'))
      if ( temp_seurat@version < 3 ) {
        temp_seurat <- SetAllIdent(temp_seurat, id = column_sample)
        temp_seurat@ident <- factor(temp_seurat@ident, levels = sample_names)
        if ( packageVersion('Seurat') < 3 ) {
          markers_by_sample <- Seurat::FindAllMarkers(
            temp_seurat,
            only.pos = only_pos,
            min.pct = min_pct,
            thresh.use = thresh_logFC,
            return.thresh = thresh_p_val,
            test.use = test,
            print.bar = verbose,
            ...
          )
        } else {
          markers_by_sample <- Seurat::FindAllMarkers(
            temp_seurat,
            only.pos = only_pos,
            min.pct = min_pct,
            logfc.threshold = thresh_logFC,
            return.thresh = thresh_p_val,
            test.use = test,
            verbose = verbose,
            ...
          )
        }
      } else {
        Idents(temp_seurat) <- factor(
          temp_seurat@meta.data[[column_sample]],
          levels = sample_names
        )
        if ( packageVersion('Seurat') < 3 ) {
          markers_by_sample <- Seurat::FindAllMarkers(
            temp_seurat,
            only.pos = only_pos,
            min.pct = min_pct,
            thresh.use = thresh_logFC,
            return.thresh = thresh_p_val,
            test.use = test,
            print.bar = verbose,
            ...
          )
        } else {
          markers_by_sample <- Seurat::FindAllMarkers(
            temp_seurat,
            only.pos = only_pos,
            min.pct = min_pct,
            logfc.threshold = thresh_logFC,
            return.thresh = thresh_p_val,
            test.use = test,
            verbose = verbose,
            ...
          )
        }
      }
      #
      if ( nrow(markers_by_sample) > 0 ) {
        markers_by_sample <- markers_by_sample %>%
          dplyr::select(c('cluster', 'gene', 'p_val', 'avg_logFC', 'pct.1', 'pct.2', 'p_val_adj')
          ) %>%
          dplyr::rename(
            sample = cluster
          )
        #
        if ( exists('genes_on_cell_surface') ) {
          markers_by_sample <- markers_by_sample %>%
            mutate(on_cell_surface = gene %in% genes_on_cell_surface)
        }
      } else {
        message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] No marker genes found for any of the samples.'))
        markers_by_sample <- 'no_markers_found'
      }
      temp_seurat@misc$marker_genes$by_sample <- markers_by_sample
    } else {
      message('Sample column provided but only 1 sample found.')
    }
  } else {
    warning(paste0('Cannot find specified column (`object@meta.data$', column_sample, '`) that is supposed to contain sample information.'))
  }
  ##--------------------------------------------------------------------------##
  ## clusters
  ## - check if column_cluster is provided and exists in meta data
  ## - get cluster names
  ## - check if more than 1 cluster exists
  ## - run Seurat::FindAllMarkers()
  ## - check if any marker genes were found
  ## - sort and rename columns
  ## - add column with cell surface genes if present
  ##--------------------------------------------------------------------------##
  #
  if ( !is.null(column_cluster) & column_cluster %in% names(temp_seurat@meta.data) ) {
    if ( is.factor(temp_seurat@meta.data[[column_cluster]]) ) {
      cluster_names <- levels(temp_seurat@meta.data[[column_cluster]])
    } else {
      cluster_names <- sort(unique(temp_seurat@meta.data[[column_cluster]]))
    }
    #
    if ( length(cluster_names) > 1 ) {
      message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Get marker genes by cluster...'))
      if ( temp_seurat@version < 3 ) {
        temp_seurat <- SetAllIdent(temp_seurat, id = column_cluster)
        temp_seurat@ident <- factor(temp_seurat@ident, levels = cluster_names)
        if ( packageVersion('Seurat') < 3 ) {
          markers_by_cluster <- Seurat::FindAllMarkers(
            temp_seurat,
            only.pos = only_pos,
            min.pct = min_pct,
            thresh.use = thresh_logFC,
            return.thresh = thresh_p_val,
            test.use = test,
            print.bar = verbose,
            ...
          )
        } else {
          markers_by_cluster <- Seurat::FindAllMarkers(
            temp_seurat,
            only.pos = only_pos,
            min.pct = min_pct,
            logfc.threshold = thresh_logFC,
            return.thresh = thresh_p_val,
            test.use = test,
            verbose = verbose,
            ...
          )
        }
      } else {
        Idents(temp_seurat) <- factor(
          temp_seurat@meta.data[[column_cluster]],
          levels = cluster_names
        )
        if ( packageVersion('Seurat') < 3 ) {
          markers_by_cluster <- Seurat::FindAllMarkers(
            temp_seurat,
            only.pos = only_pos,
            min.pct = min_pct,
            thresh.use = thresh_logFC,
            return.thresh = thresh_p_val,
            test.use = test,
            print.bar = verbose,
            ...
          )
        } else {
          markers_by_cluster <- Seurat::FindAllMarkers(
            temp_seurat,
            only.pos = only_pos,
            min.pct = min_pct,
            logfc.threshold = thresh_logFC,
            return.thresh = thresh_p_val,
            test.use = test,
            verbose = verbose,
            ...
          )
        }
      }
      #
      if ( nrow(markers_by_cluster) > 0 ) {
        markers_by_cluster <- markers_by_cluster %>%
          dplyr::select(c('cluster', 'gene', 'p_val', 'avg_logFC', 'pct.1', 'pct.2', 'p_val_adj')
          )
        #
        if ( exists('genes_on_cell_surface') ) {
          markers_by_cluster <- markers_by_cluster %>%
            mutate(on_cell_surface = gene %in% genes_on_cell_surface)
        }
      } else {
        message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] No marker genes found for any of the clusters.'))
        markers_by_cluster <- 'no_markers_found'
      }
      temp_seurat@misc$marker_genes$by_cluster <- markers_by_cluster
    } else {
      message('Cluster column provided but only 1 cluster found.')
    }
  } else {
    warning(paste0('Cannot find specified column (`object@meta.data$', column_cluster, '`) that is supposed to contain cluster information.'))
  }
  ##--------------------------------------------------------------------------##
  ## return Seurat object
  ##--------------------------------------------------------------------------##
  return(temp_seurat)
}










