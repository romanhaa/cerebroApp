#' Get enriched pathways based on marker genes from EnrichR.
#' @title Get enriched pathways based on marker genes from EnrichR.
#' @description This function uses the enrichR API to look for enriched pathways
#' in marker gene sets of samples and clusters.
#' @param object Seurat object.
#' @param column_sample Column in object@meta.data that contains information
#' about sample; defaults to 'sample'.
#' @param column_cluster Column in object@meta.data that contains information
#' about cluster; defaults to 'cluster'.
#' @param databases Which databases to query. Use enrichR::listEnrichrDbs() to
#' check what databases are available.
#' @param adj_p_cutoff Cut-off for adjusted p-value of enriched pathways;
#' defaults to 0.05,
#' @param max_terms Save only first n entries of each database; defaults to 100.
#' @param URL_API URL to send requests to (Enrichr API). Allows to overwrite
#' default URL with an alternative taken from the Enrichr website in case the
#' original is out-of-service; defaults to
#' 'http://amp.pharm.mssm.edu/Enrichr/enrich'.
#' @keywords seurat cerebro
#' @export
#' @import dplyr
#' @examples
#' seurat <- getEnrichedPathways(
#'   object = seurat,
#'   column_sample = 'sample',
#'   column_cluster = 'cluster',
#'   databases = c('GO_Biological_Process_2018','GO_Cellular_Component_2018'),
#'   adj_p_cutoff = 0.01,
#'   max_terms = 100,
#'   URL_API = 'http://amp.pharm.mssm.edu/Enrichr/enrich'
#' )
getEnrichedPathways <- function(
  object,
  column_sample = 'sample',
  column_cluster = 'cluster',
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
  # check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Package \"Seurat\" needed for this function to work. Please install it.",
      call. = FALSE)
  }
  ##--------------------------------------------------------------------------##
  ## create backup of Seurat object (probably not necessary)
  ##--------------------------------------------------------------------------##
  temp_seurat <- object
  ##--------------------------------------------------------------------------##
  ## check if marker genes are present and stop if they aren't
  ##--------------------------------------------------------------------------##
  if ( is.null(temp_seurat@misc$marker_genes) ) {
    stop("Please run 'getMarkerGenes()' first.", call. = FALSE)
  }
  ##--------------------------------------------------------------------------##
  ## samples
  ## - check if marker genes by sample are available
  ## - extract marker genes by sample
  ## - get sample names and remove those for which no marker genes are available
  ## - create slot for annotation if doesn't already exist
  ## - annotate marker genes for each sample in parallel
  ## - try up to three times to run enrichR annotation (fails sometimes)
  ## - filter results
  ##--------------------------------------------------------------------------##
  if ( !is.null(temp_seurat@misc$marker_genes$by_sample) ) {
    if ( is.data.frame(temp_seurat@misc$marker_genes$by_sample) ) {
      message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Get enriched pathway for samples...'))
      #
      markers_by_sample <- temp_seurat@misc$marker_genes$by_sample
      #
      if ( is.factor(temp_seurat@meta.data[[column_sample]]) ) {
        sample_names <- levels(temp_seurat@meta.data[[column_sample]])
      } else {
        sample_names <- unique(temp_seurat@meta.data[[column_sample]])
      }
      # remove samples for which no marker genes were found
      sample_names <- sample_names[which(sample_names %in% unique(markers_by_sample$sample))]

      #
      results_by_sample <- future.apply::future_sapply(
        sample_names, USE.NAMES = TRUE, simplify = FALSE, future.globals = FALSE, function(x) {
        temp <- list()
        attempt <- 1
        while( length(temp) == 0 && !('Adjusted.P.value' %in% names(temp)) && attempt <= 3 ) {
          attempt <- attempt + 1
          try(
            temp <- markers_by_sample %>%
              dplyr::filter(sample == x) %>%
              dplyr::select('gene') %>%
              t() %>%
              as.vector() %>%
              send_enrichr_query(databases = databases, URL_API = URL_API)
          )
        }
        #
        results_2 <- sapply(names(temp), USE.NAMES = TRUE, simplify = FALSE, function(y) {
          length <- temp[[y]] %>% dplyr::filter(Adjusted.P.value <= adj_p_cutoff) %>% nrow()
          # if there are more than max_terms entries with an adjusted p-value of 1 or less...
          if ( length > max_terms ) {
            temp[[y]] %>%
            dplyr::top_n(-max_terms, Adjusted.P.value) %>%
            dplyr::mutate(db = y)
          # if there is at least 1 entry with an adjusted p-value of 1 or less...
          } else if ( length > 0 ) {
            temp[[y]] %>%
            dplyr::filter(Adjusted.P.value <= adj_p_cutoff) %>%
            dplyr::mutate(db = y)
          # remove the current database
          } else {
            NULL
          }
        })
        # remove samples without any enriched pathways (in any database)
        for ( i in names(results_2) ) {
          if ( is.null(results_2[[i]]) ) {
            results_2[[i]] <- NULL
          } else {
            results_2 <- do.call(rbind, results_2)
          }
        }
        return(results_2)
      })
      for ( i in names(results_by_sample) ) {
        results_by_sample[[i]] <- results_by_sample[[i]] %>%
          dplyr::mutate(sample = i)
      }
      results_by_sample <- do.call(rbind, results_by_sample) %>%
        dplyr::select(sample, db, dplyr::everything()) %>%
        dplyr::mutate(
          sample = factor(sample, levels = sample_names),
          db = factor(db, databases)
        )
    } else if ( temp_seurat@misc$marker_genes$by_sample == 'no_markers_found' ) {
      message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Skipping pathway enrichment for samples because no marker genes were identified for any sample.'))
      results_by_sample <- 'no_markers_found'
    } else {
      warning('Unexpected data format of marker genes for samples. Please submit an issue on GitHub: https://github.com/romanhaa/cerebroPrepare.')
    }
  } else {
    warning('No marker genes for samples available.')
  }
  ##--------------------------------------------------------------------------##
  ## clusters
  ## - check if marker genes by cluster are available
  ## - extract marker genes by cluster
  ## - get cluster names and remove those for which no marker genes are available
  ## - create slot for annotation if doesn't already exist
  ## - annotate marker genes for each cluster in parallel
  ## - try up to three times to run enrichR annotation (fails sometimes)
  ## - filter results
  ##--------------------------------------------------------------------------##
  if ( !is.null(temp_seurat@misc$marker_genes$by_cluster) ) {
    if ( is.data.frame(temp_seurat@misc$marker_genes$by_cluster) ) {
      message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Get enriched pathway for clusters...'))
      #
      markers_by_cluster <- temp_seurat@misc$marker_genes$by_cluster
      #
      if ( is.factor(temp_seurat@meta.data[[column_cluster]]) ) {
        cluster_names <- as.character(levels(temp_seurat@meta.data[[column_cluster]]))
      } else {
        cluster_names <- sort(unique(temp_seurat@meta.data[[column_cluster]]))
      }
      # remove clusters for which no marker genes were found
      cluster_names <- cluster_names[which(cluster_names %in% unique(markers_by_cluster$cluster))]

      #
      results_by_cluster <- future.apply::future_sapply(
        cluster_names, USE.NAMES = TRUE, simplify = FALSE, future.globals = FALSE, function(x) {
        temp <- list()
        attempt <- 1
        while( length(temp) == 0 && !('Adjusted.P.value' %in% names(temp)) && attempt <= 3 ) {
          attempt <- attempt + 1
          try(
            temp <- markers_by_cluster %>%
              dplyr::filter(cluster == x) %>%
              dplyr::select('gene') %>%
              t() %>%
              as.vector() %>%
              send_enrichr_query(databases = databases, URL_API = URL_API)
          )
        }
        #
        results_2 <- sapply(names(temp), USE.NAMES = TRUE, simplify = FALSE, function(y) {
          length <- temp[[y]] %>% dplyr::filter(Adjusted.P.value <= adj_p_cutoff) %>% nrow()
          # if there are more than max_terms entries with an adjusted p-value of 1 or less...
          if ( length > max_terms ) {
            temp[[y]] %>%
            dplyr::top_n(-max_terms, Adjusted.P.value) %>%
            dplyr::mutate(db = y)
          # if there is at least 1 entry with an adjusted p-value of 1 or less...
          } else if ( length > 0 ) {
            temp[[y]] %>%
            dplyr::filter(Adjusted.P.value <= adj_p_cutoff) %>%
            dplyr::mutate(db = y)
          # remove the curent database
          } else {
            NULL
          }
        })
        # remove samples without any enriched pathways (in any database)
        for ( i in names(results_2) ) {
          if ( is.null(results_2[[i]]) ) {
            results_2[[i]] <- NULL
          } else {
            results_2 <- do.call(rbind, results_2)
          }
        }
        return(results_2)
      })
      for ( i in names(results_by_cluster) ) {
        results_by_cluster[[i]] <- results_by_cluster[[i]] %>%
          dplyr::mutate(cluster = i)
      }
      results_by_cluster <- do.call(rbind, results_by_cluster) %>%
        dplyr::select(cluster, db, dplyr::everything()) %>%
        dplyr::mutate(
          cluster = factor(cluster, levels = cluster_names),
          db = factor(db, levels = databases)
        )
    } else if ( temp_seurat@misc$marker_genes$by_cluster == 'no_markers_found' ) {
      message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Skipping pathway enrichment for cluster because no marker genes were identified for any cluster.'))
      results_by_cluster <- 'no_markers_found'
    } else {
      warning('Unexpected data format of marker genes for clusters. Please submit an issue on GitHub: https://github.com/romanhaa/cerebroPrepare.')
    }
  } else {
    warning('No marker genes for clusters available.')
  }

  #----------------------------------------------------------------------------#
  # merge results, add to Seurat object and return Seurat object
  #----------------------------------------------------------------------------#
  results <- list(
    by_sample = results_by_sample,
    by_cluster = results_by_cluster,
    parameters = list(
      databases = databases,
      adj_p_cutoff = adj_p_cutoff,
      max_terms = max_terms
    )
  )
  temp_seurat@misc$enriched_pathways$enrichr <- results

  ##--------------------------------------------------------------------------##
  ## return Seurat object
  ##--------------------------------------------------------------------------##
  return(temp_seurat)
}
