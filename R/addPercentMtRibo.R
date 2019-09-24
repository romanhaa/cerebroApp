#' Add percentage of mitochondrial and ribosomal transcripts.
#' @title Add percentage of mitochondrial and ribosomal transcripts.
#' @description Get percentage of transcripts of gene list compared to all
#' transcripts per cell.
#' @keywords Cerebro scRNAseq Seurat
#' @param object Seurat object.
#' @param organism Organism, can be either human ('hg') or mouse ('mm'). Genes
#' need to annotated as gene symbol, e.g. MKI67 (human) / Mki67 (mouse).
#' @param gene_nomenclature Define if genes are saved by their name ('name'),
#' ENSEMBL ID ('ensembl') or GENCODE ID ('gencode_v27', 'gencode_vM16').
#' @keywords seurat cerebro
#' @export
#' @return Seurat object with two new meta data columns containing the
#' percentage of mitochondrial and ribosomal gene expression for each cell.
#' @import dplyr
#' @examples
#' seurat <- addPercentMtRibo(
#'   object = seurat,
#'   organism = 'hg',
#'   gene_nomenclature = 'name'
#' )
addPercentMtRibo <- function(
  object,
  organism,
  gene_nomenclature
) {
  # check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Package 'Seurat' needed for this function to work. Please install it.", call. = FALSE)
  }
  ##--------------------------------------------------------------------------##
  ## check if organism is supported
  ##--------------------------------------------------------------------------##
  supported_organisms <- c('hg','mm')
  if ( !(organism %in% supported_organisms) ) {
    stop(
      paste0(
        "User-specified organism ('", organism ,
        "') not in list of supported organisms: ",
        paste(supported_organisms, collapse = ', ')
      )
    )
  }
  supported_nomenclatures <- c('name','ensembl','gencode_v27','gencode_vM16')
  if ( !(gene_nomenclature %in% supported_nomenclatures) ) {
    stop(
      paste0(
        "User-specified gene nomenclature ('", gene_nomenclature,
        "') not in list of supported nomenclatures: ",
        paste(supported_nomenclatures, collapse = ', ')
      )
    )
  }
  ##--------------------------------------------------------------------------##
  ## load mitochondrial and ribosomal gene lists from extdata
  ##--------------------------------------------------------------------------##
  genes_mt <- readr::read_tsv(
      system.file(
        'extdata',
        paste0('genes_mt_', organism, '_', gene_nomenclature, '.txt'),
        package = 'cerebroApp'
      ),
      col_types = readr::cols(),
      col_names = FALSE
    ) %>%
    dplyr::select(1) %>%
    t() %>%
    as.vector()
  genes_ribo <- readr::read_tsv(
      system.file(
        'extdata',
        paste0('genes_ribo_', organism, '_', gene_nomenclature, '.txt'),
        package = 'cerebroApp'
      ),
      col_types = readr::cols(),
      col_names = FALSE
    ) %>%
    dplyr::select(1) %>%
    t() %>%
    as.vector()
  ##--------------------------------------------------------------------------##
  ## keep only genes that are present in data set
  ##--------------------------------------------------------------------------##
  if ( object@version < 3 ) {
    genes_mt_here <- intersect(genes_mt, rownames(object@raw.data))
    genes_ribo_here <- intersect(genes_ribo, rownames(object@raw.data))
  } else {
    genes_mt_here <- intersect(genes_mt, rownames(object@assays$RNA@counts))
    genes_ribo_here <- intersect(genes_ribo, rownames(object@assays$RNA@counts))
  }
  ##--------------------------------------------------------------------------##
  ## save gene lists in Seurat object and create place if not existing yet
  ##--------------------------------------------------------------------------##
  if ( is.null(object@misc$gene_lists) ) {
    object@misc$gene_lists <- list()
  }
  object@misc$gene_lists$mitochondrial_genes <- genes_mt_here
  object@misc$gene_lists$ribosomal_genes <- genes_ribo_here
  ##--------------------------------------------------------------------------##
  ## calculate percentage of transcripts for mitochondrial and ribosomal genes
  ##--------------------------------------------------------------------------##
  message(paste0('[', format(Sys.time(), '%H:%M:%S'), '] Calculate percentage of mitochondrial and ribosomal transcripts...'))
  values <- cerebroApp::calculatePercentGenes(
    object,
    list(
      'genes_mt' = genes_mt_here,
      'genes_ribo' = genes_ribo_here
    )
  )
  ##--------------------------------------------------------------------------##
  ## add results to Seurat object
  ##--------------------------------------------------------------------------##
  if ( object@version < 3 ) {
    object <- Seurat::AddMetaData(
      object,
      data.frame(
        row.names = colnames(object@raw.data),
        'percent_mt' = values[['genes_mt']],
        'percent_ribo' = values[['genes_ribo']]
      )
    )
  } else {
    object$percent_mt <- values[['genes_mt']]
    object$percent_ribo <- values[['genes_ribo']]
  }
  ##--------------------------------------------------------------------------##
  ##
  ##--------------------------------------------------------------------------##
  return(object)
}









