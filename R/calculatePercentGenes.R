#' Calculate percentage of transcripts of gene list.
#' @title Calculate percentage of transcripts of gene list.
#' @description Get percentage of transcripts of gene list compared to all
#' transcripts per cell.
#' @param object Seurat object.
#' @param genes List(s) of genes.
#' @keywords seurat cerebro
#' @export
#' @import Seurat
#' @examples
#' calculatePercentGenes(
#'   object = seurat,
#'   genes = list('example' = c('Fth1','Atf1'))
#' )
calculatePercentGenes <- function(
  object,
  genes
) {
  ##--------------------------------------------------------------------------##
  ## get for every supplied gene list, get the genes that are present in the
  ## data set and calculate the percentage of transcripts that they account for
  ##--------------------------------------------------------------------------##
  if ( object@version < 3 ) {
    result <- pbapply::pblapply(
      genes,
      function(x) {
        genes_here <- intersect(x, rownames(object@raw.data))
        Matrix::colSums(object@raw.data[genes_here,]) / Matrix::colSums(object@raw.data)
      }
    )
  } else {
    result <- pbapply::pblapply(
      genes,
      function(x) {
        genes_here <- intersect(x, rownames(object@assays$RNA@counts))
        Matrix::colSums(object@assays$RNA@counts[genes_here,]) / Matrix::colSums(object@assays$RNA@counts)
      }
    )
  }
  ##--------------------------------------------------------------------------##
  ## return list with results
  ##--------------------------------------------------------------------------##
  return(result)
}
