#' Get marker genes for every sample and cluster in Seurat object.
#' @title Get marker genes for every sample and cluster in Seurat object.
#' @description This function gets marker genes for every sample and cluster of
#' the Seurat object.
#' @keywords Cerebro scRNAseq Seurat
#' @param object Seurat object.
#' @param assay Assay to pull counts from; defaults to 'RNA'. Only relevant in
#' Seurat v3.0 or higher since the concept of assays wasn't implemented before.
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
#' @export
#' @return Seurat object with marker gene results for samples and clusters
#' stored in object@misc$marker_genes.
#' @import dplyr
#' @importFrom rlang .data
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.2/seurat_pbmc.rds",
#'   package = "cerebroApp"))
#' pbmc <- getMarkerGenes(
#'   object = pbmc,
#'   organism = 'hg',
#'   column_sample = 'sample',
#'   column_cluster = 'seurat_clusters',
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
  ## check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE))
  {
    stop(
      "Package 'Seurat' needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  ## check if sample column has been specified and exists in meta data
  if (
    is.null(column_sample) |
    (column_sample %in% names(object@meta.data) == FALSE)
  )
  {
    stop(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Cannot find specified column (`object@meta.data$', column_sample,
        '`) that is supposed to contain sample information.'
      ),
      call. = FALSE
    )
  }
  ## check if cluster column has been specified and exists in meta data
  if (
    is.null(column_cluster) |
    (column_cluster %in% names(object@meta.data) == FALSE)
  )
  {
    stop(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Cannot find specified column (`object@meta.data$', column_cluster,
        '`) that is supposed to contain cluster information.'
      ),
      call. = FALSE
    )
  }
  ##--------------------------------------------------------------------------##
  ## Get list of genes in cell surface through gene ontology term GO:0009986.
  ##--------------------------------------------------------------------------##
  if ( organism %in% c('hg','mm') == FALSE )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] No information about genes on cell surface because organism is ',
        'either not specified or not human/mouse.'
      )
    )
  } else
  {
    if ( organism == 'hg' || organism == 'human' )
    {
      temp_attributes <- 'hgnc_symbol'
      temp_dataset <- 'hsapiens_gene_ensembl'
    } else if ( organism == 'mm' || organism == 'mouse' )
    {
      temp_attributes <- 'external_gene_name'
      temp_dataset <- 'mmusculus_gene_ensembl'
    }
    attempt <- 1
    while(
      !exists('genes_on_cell_surface') &&
      attempt <= 3
    )
    {
      try(
        genes_on_cell_surface <- biomaRt::getBM(
          attributes = temp_attributes,
          filters = 'go',
          values = 'GO:0009986',
          mart = biomaRt::useMart('ensembl', dataset = temp_dataset)
        )[,1]
      )
    }
    if ( !exists('genes_on_cell_surface') )
    {
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
  ## samples
  ## - get sample names
  ## - check if more than 1 sample exists
  ## - run Seurat::FindAllMarkers()
  ## - check if any marker genes were found
  ## - sort and rename columns
  ## - add column with cell surface genes if present
  ## - store results in Seurat object
  ##--------------------------------------------------------------------------##
  #
  if ( is.factor(object@meta.data[[column_sample]]) )
  {
    sample_names <- levels(object@meta.data[[column_sample]])
  } else {
    sample_names <- unique(object@meta.data[[column_sample]])
  }
  #
  if ( length(sample_names) > 1 )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] Get marker genes for samples...'
      )
    )
    if ( object@version < 3 )
    {
      object <- Seurat::SetAllIdent(object, id = column_sample)
      object@ident <- factor(object@ident, levels = sample_names)
      if ( utils::packageVersion('Seurat') < 3 )
      {
        markers_by_sample <- Seurat::FindAllMarkers(
          object,
          only.pos = only_pos,
          min.pct = min_pct,
          thresh.use = thresh_logFC,
          return.thresh = thresh_p_val,
          test.use = test,
          print.bar = verbose,
          ...
        )
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
        markers_by_sample <- Seurat::FindAllMarkers(
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
      }
    } else
    {
      Seurat::Idents(object) <- factor(
        object@meta.data[[column_sample]],
        levels = sample_names
      )
      if ( utils::packageVersion('Seurat') < 3 )
      {
        markers_by_sample <- Seurat::FindAllMarkers(
          object,
          only.pos = only_pos,
          min.pct = min_pct,
          thresh.use = thresh_logFC,
          return.thresh = thresh_p_val,
          test.use = test,
          print.bar = verbose,
          ...
        )
      } else
      {
        markers_by_sample <- Seurat::FindAllMarkers(
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
      }
    }
    #
    if ( nrow(markers_by_sample) > 0 )
    {
      markers_by_sample <- markers_by_sample %>%
        dplyr::select(c('cluster','gene','p_val','avg_logFC','pct.1','pct.2',
          'p_val_adj')) %>%
        dplyr::rename(sample = .data$cluster)
      #
      if ( exists('genes_on_cell_surface') )
      {
        markers_by_sample <- markers_by_sample %>%
          dplyr::mutate(on_cell_surface = .data$gene %in% genes_on_cell_surface)
      }
    } else
    {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] No marker genes found for any of the samples.'
        )
      )
      markers_by_sample <- 'no_markers_found'
    }
  } else
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Sample column provided but only 1 sample found.'
      )
    )
    markers_by_sample <- 'no_markers_found'
  }
  ##--------------------------------------------------------------------------##
  ## clusters
  ## - get cluster names
  ## - check if more than 1 cluster exists
  ## - run Seurat::FindAllMarkers()
  ## - check if any marker genes were found
  ## - sort and rename columns
  ## - add column with cell surface genes if present
  ##--------------------------------------------------------------------------##
  #
  if ( is.factor(object@meta.data[[column_cluster]]) )
  {
    cluster_names <- levels(object@meta.data[[column_cluster]])
  } else
  {
    cluster_names <- unique(object@meta.data[[column_cluster]]) %>% sort()
  }
  #
  if ( length(cluster_names) > 1 )
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] Get marker genes by cluster...'
      )
    )
    if ( object@version < 3 )
    {
      object <- Seurat::SetAllIdent(object, id = column_cluster)
      object@ident <- factor(object@ident, levels = cluster_names)
      if ( utils::packageVersion('Seurat') < 3 )
      {
        markers_by_cluster <- Seurat::FindAllMarkers(
          object,
          only.pos = only_pos,
          min.pct = min_pct,
          thresh.use = thresh_logFC,
          return.thresh = thresh_p_val,
          test.use = test,
          print.bar = verbose,
          ...
        )
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
        markers_by_cluster <- Seurat::FindAllMarkers(
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
      }
    } else
    {
      Seurat::Idents(object) <- factor(
        object@meta.data[[column_cluster]],
        levels = cluster_names
      )
      if ( utils::packageVersion('Seurat') < 3 )
      {
        markers_by_cluster <- Seurat::FindAllMarkers(
          object,
          only.pos = only_pos,
          min.pct = min_pct,
          thresh.use = thresh_logFC,
          return.thresh = thresh_p_val,
          test.use = test,
          print.bar = verbose,
          ...
        )
      } else
      {
        markers_by_cluster <- Seurat::FindAllMarkers(
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
      }
    }
    #
    if ( nrow(markers_by_cluster) > 0 )
    {
      markers_by_cluster <- markers_by_cluster %>%
        dplyr::select(c('cluster','gene','p_val','avg_logFC','pct.1','pct.2',
          'p_val_adj'))
      #
      if ( exists('genes_on_cell_surface') )
      {
        markers_by_cluster <- markers_by_cluster %>%
          dplyr::mutate(on_cell_surface = .data$gene %in% genes_on_cell_surface)
      }
    } else
    {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] No marker genes found for any of the clusters.'
        )
      )
      markers_by_cluster <- 'no_markers_found'
    }
  } else
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] Cluster column provided but only 1 cluster found.'
      )
    )
    markers_by_cluster <- 'no_markers_found'
  }
  ##---------------------------------------------------------------------------#
  ## merge results, add to Seurat object and return Seurat object
  ##---------------------------------------------------------------------------#
  results <- list(
    by_sample = markers_by_sample,
    by_cluster = markers_by_cluster,
    parameters = list(
      only_positive = only_pos,
      minimum_percentage = min_pct,
      logFC_threshold = thresh_logFC,
      p_value_threshold = thresh_p_val,
      test = test
    )
  )
  object@misc$marker_genes <- results
  ##--------------------------------------------------------------------------##
  ## return Seurat object
  ##--------------------------------------------------------------------------##
  return(object)
}










