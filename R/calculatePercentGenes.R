#' Calculate percentage of transcripts of gene list.
#' @title Calculate percentage of transcripts of gene list.
#' @description Get percentage of transcripts of gene list compared to all
#' transcripts per cell.
#' @keywords Cerebro scRNAseq Seurat
#' @param object Seurat object.
#' @param genes List(s) of genes.
#' @export
#' @return List of lists containing the percentages of expression for each
#' provided gene list.
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.2/seurat_pbmc.rds",
#'   package = "cerebroApp"))
#' calculatePercentGenes(
#'   object = pbmc,
#'   genes = list('example' = c('FCN1','CD3D'))
#' )
calculatePercentGenes <- function(
  object,
  genes
) {
  # check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop(
      "Package 'Seurat' needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  ##--------------------------------------------------------------------------##
  ## get for every supplied gene list, get the genes that are present in the
  ## data set and calculate the percentage of transcripts that they account for
  ##--------------------------------------------------------------------------##
  if ( object@version < 3 ) {
    result <- pbapply::pblapply(
      genes,
      function(x) {
        genes_here <- intersect(x, rownames(object@raw.data))
        if ( length(genes_here) == 1 ) {
          object@raw.data[genes_here,] / Matrix::colSums(object@raw.data)
        } else {
          Matrix::colSums(object@raw.data[genes_here,]) /
          Matrix::colSums(object@raw.data)
        }
      }
    )
  } else {
    result <- pbapply::pblapply(
      genes,
      function(x) {
        genes_here <- intersect(x, rownames(object@assays$RNA@counts))
        if ( length(genes_here) == 1 ) {
          object@assays$RNA@counts[genes_here,] /
          Matrix::colSums(object@assays$RNA@counts)
        } else {
          Matrix::colSums(object@assays$RNA@counts[genes_here,]) /
          Matrix::colSums(object@assays$RNA@counts)
        }
      }
    )
  }
  ##--------------------------------------------------------------------------##
  ## return list with results
  ##--------------------------------------------------------------------------##
  return(result)
}
