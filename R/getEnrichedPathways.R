#' Get enriched pathways based on marker genes from EnrichR.
#' @title Get enriched pathways based on marker genes from EnrichR.
#' @description This function uses the enrichR API to look for enriched pathways
#' in marker gene sets of samples and clusters.
#' @keywords Cerebro scRNAseq Seurat Enrichr
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
#' @export
#' @return Seurat object with Enrichr results for samples and clusters stored in
#' object@misc$enriched_pathways$enrichr
#' @import dplyr
#' @importFrom rlang .data
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.2/seurat_pbmc.rds",
#'   package = "cerebroApp"))
#' pbmc <- getEnrichedPathways(
#'   object = pbmc,
#'   column_sample = 'sample',
#'   column_cluster = 'seurat_clusters',
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
  ## check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE))
  {
    stop(
      "Package 'Seurat' needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  ##--------------------------------------------------------------------------##
  ## check if marker genes are present and stop if they aren't
  ##--------------------------------------------------------------------------##
  if ( is.null(object@misc$marker_genes) )
  {
    stop(
      "No marker genes found. Please run 'getMarkerGenes()' first.",
      call. = FALSE
    )
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
  if ( !is.null(object@misc$marker_genes$by_sample) )
  {
    if ( is.data.frame(object@misc$marker_genes$by_sample) )
    {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Get enriched pathway for samples...'
        )
      )
      #
      markers_by_sample <- object@misc$marker_genes$by_sample
      #
      if ( is.factor(object@meta.data[[column_sample]]) )
      {
        sample_names <- levels(object@meta.data[[column_sample]])
      } else
      {
        sample_names <- unique(object@meta.data[[column_sample]])
      }
      ## remove samples for which no marker genes were found
      sample_names <- intersect(sample_names, unique(markers_by_sample$sample))

      #
      results_by_sample <- future.apply::future_sapply(
        sample_names, USE.NAMES = TRUE, simplify = FALSE,
        future.globals = FALSE, function(x)
      {
        temp <- list()
        attempt <- 1
        while(
          'Adjusted.P.value' %in% names(temp) == FALSE &&
          attempt <= 3
        )
        {
          attempt <- attempt + 1
          try(
            temp <- markers_by_sample %>%
              dplyr::filter(.data$sample == x) %>%
              dplyr::select('gene') %>%
              t() %>%
              as.vector() %>%
              .send_enrichr_query(databases = databases, URL_API = URL_API)
          )
        }
        #
        results_2 <- sapply(names(temp), USE.NAMES = TRUE,
          simplify = FALSE, function(y)
        {
          ## apply cut-off of adj. p-value and add database info as column
          out <- temp[[y]] %>%
            dplyr::filter(.data$Adjusted.P.value <= adj_p_cutoff) %>%
            dplyr::mutate(db = y)
          ## if there are more than max_terms entries...
          if ( nrow(out) > max_terms )
          {
            out <- out %>% dplyr::top_n(-max_terms, .data$Adjusted.P.value)
          ## if there are no entries left
          } else if ( nrow(out) == 0 )
          {
            out <- NULL
          }
          return(out)
        })
        ## remove dbs without any enriched entries
        for ( i in names(results_2) )
        {
          if ( is.null(results_2[[i]]) ) results_2[[i]] <- NULL
        }
        ## merge databases within each sample
        results_2 <- do.call(rbind, results_2)
        return(results_2)
      })
      ## remove samples without any enriched entry in any database
      for ( i in names(results_by_sample) )
      {
        if ( is.null(results_by_sample[[i]]) ) results_by_sample[[i]] <- NULL
      }
      ## add sample info as column
      for ( i in names(results_by_sample) )
      {
        results_by_sample[[i]] <- results_by_sample[[i]] %>%
          dplyr::mutate(sample = i)
      }
      ## check if all databases returned empty lists
      if ( length(results_by_sample) == 0 ) {
        results_by_sample <- 'no_markers_found'
        message(
          paste0(
            '[', format(Sys.time(), '%H:%M:%S'), '] 0 pathways passed the ',
            'thresholds across all samples and databases.'
          )
        )
      } else {
        ## merge samples into single table
        results_by_sample <- do.call(rbind, results_by_sample) %>%
          dplyr::select('sample', 'db', dplyr::everything()) %>%
          dplyr::mutate(
            sample = factor(.data$sample, levels = intersect(sample_names,
              .data$sample)),
            db = factor(.data$db, databases)
          )
        message(
          paste0(
            '[', format(Sys.time(), '%H:%M:%S'), '] ', nrow(results_by_sample),
            ' pathways passed the thresholds across all samples and databases.'
          )
        )
      }
    } else if ( object@misc$marker_genes$by_sample == 'no_markers_found' )
    {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Skipping pathway enrichment for samples because no marker genes ',
          'were identified for any sample.'
        )
      )
      results_by_sample <- 'no_markers_found'
    } else
    {
      warning(
        paste0(
          'Unexpected data format of marker genes for samples. Please submit ',
          'an issue on GitHub: https://github.com/romanhaa/cerebroApp.'
        )
      )
    }
  } else
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] No marker genes for samples available.'
      )
    )
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
  if ( !is.null(object@misc$marker_genes$by_cluster) )
  {
    if ( is.data.frame(object@misc$marker_genes$by_cluster) )
    {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Get enriched pathway for clusters...'
        )
      )
      #
      markers_by_cluster <- object@misc$marker_genes$by_cluster
      #
      if ( is.factor(object@meta.data[[column_cluster]]) )
      {
        cluster_names <- as.character(levels(object@meta.data[[column_cluster]]))
      } else
      {
        cluster_names <- sort(unique(object@meta.data[[column_cluster]]))
      }
      ## remove clusters for which no marker genes were found
      cluster_names <- intersect(cluster_names, unique(markers_by_cluster$cluster))

      #
      results_by_cluster <- future.apply::future_sapply(
        cluster_names, USE.NAMES = TRUE, simplify = FALSE,
        future.globals = FALSE, function(x)
      {
        temp <- list()
        attempt <- 1
        while(
          'Adjusted.P.value' %in% names(temp) == FALSE &&
          attempt <= 3
        )
        {
          attempt <- attempt + 1
          try(
            temp <- markers_by_cluster %>%
              dplyr::filter(.data$cluster == x) %>%
              dplyr::select('gene') %>%
              t() %>%
              as.vector() %>%
              .send_enrichr_query(databases = databases, URL_API = URL_API)
          )
        }
        #
        results_2 <- sapply(names(temp), USE.NAMES = TRUE,
          simplify = FALSE, function(y)
        {
          ## apply cut-off of adj. p-value and add database info as column
          out <- temp[[y]] %>%
            dplyr::filter(.data$Adjusted.P.value <= adj_p_cutoff) %>%
            dplyr::mutate(db = y)
          ## if there are more than max_terms entries...
          if ( nrow(out) > max_terms )
          {
            out <- out %>% dplyr::top_n(-max_terms, .data$Adjusted.P.value)
          ## if there are no entries left
          } else if ( nrow(out) == 0 )
          {
            out <- NULL
          }
          return(out)
        })
        ## remove dbs without any enriched entries
        for ( i in names(results_2) )
        {
          if ( is.null(results_2[[i]]) ) results_2[[i]] <- NULL
        }
        ## merge databases within each cluster
        results_2 <- do.call(rbind, results_2)
        return(results_2)
      })
      ## remove clusters without any enriched entry in any database
      for ( i in names(results_by_cluster) )
      {
        if ( is.null(results_by_cluster[[i]]) ) results_by_cluster[[i]] <- NULL
      }
      ## add cluster info as column
      for ( i in names(results_by_cluster) )
      {
        results_by_cluster[[i]] <- results_by_cluster[[i]] %>%
          dplyr::mutate(cluster = i)
      }
      ## check if all databases returned empty lists
      if ( length(results_by_cluster) == 0 ) {
        results_by_cluster <- 'no_markers_found'
        message(
          paste0(
            '[', format(Sys.time(), '%H:%M:%S'), '] 0 pathways passed the ',
            'thresholds across all samples and databases.'
          )
        )
      } else {
        ## merge clusters into single table
        results_by_cluster <- do.call(rbind, results_by_cluster) %>%
          dplyr::select(.data$cluster, .data$db, dplyr::everything()) %>%
          dplyr::mutate(
            cluster = factor(.data$cluster, levels = intersect(cluster_names,
              .data$cluster)),
            db = factor(.data$db, databases)
          )
        message(
          paste0(
            '[', format(Sys.time(), '%H:%M:%S'), '] ', nrow(results_by_cluster),
            ' pathways passed the thresholds across all clusters and databases.'
          )
        )
      }
    } else if ( object@misc$marker_genes$by_cluster == 'no_markers_found' )
    {
      message(
        paste0(
          '[', format(Sys.time(), '%H:%M:%S'),
          '] Skipping pathway enrichment for cluster because no marker genes ',
          'were identified for any cluster.'
        )
      )
      results_by_cluster <- 'no_markers_found'
    } else
    {
      warning(
        paste0(
          'Unexpected data format of marker genes for clusters. Please submit ',
          'an issue on GitHub: https://github.com/romanhaa/cerebroApp.'
        )
      )
    }
  } else
  {
    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'),
        '] No marker genes for clusters available.'
      )
    )
  }
  ##---------------------------------------------------------------------------#
  ## merge results, add to Seurat object and return Seurat object
  ##---------------------------------------------------------------------------#
  results <- list(
    by_sample = results_by_sample,
    by_cluster = results_by_cluster,
    parameters = list(
      databases = databases,
      adj_p_cutoff = adj_p_cutoff,
      max_terms = max_terms
    )
  )
  object@misc$enriched_pathways$enrichr <- results

  ##--------------------------------------------------------------------------##
  ## return Seurat object
  ##--------------------------------------------------------------------------##
  return(object)
}
