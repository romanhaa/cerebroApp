#' Calculate percentage of transcripts of gene list.
#' @title Calculate percentage of transcripts of gene list.
#' @description Get percentage of transcripts of gene list compared to all
#' transcripts per cell.
#' @keywords Cerebro scRNAseq Seurat
#' @param object Seurat object.
#' @param assay Assay to pull counts from; defaults to 'RNA'. Only relevant in
#' Seurat v3.0 or higher since the concept of assays wasn't implemented before.
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
  assay = 'RNA',
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
    # check if `raw.data` matrix exist in provided Seurat object
    if ( ('raw.data' %in% names(object) == FALSE ) ) {
      stop(
        paste0(
          '`raw.data` matrix could not be found in provided Seurat ',
          'object.'
        ),
        call. = FALSE
      )
    }
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
    # check if provided assay exists
    if ( (assay %in% names(object@assays) == FALSE ) ) {
      stop(
        paste0(
          'Assay slot `', assay, '` could not be found in provided Seurat ',
          'object.'
        ),
        call. = FALSE
      )
    }
    # check if `counts` matrix exist in provided assay
    if ( is.null(object@assays[[assay]]@counts) ) {
      stop(
        paste0(
          '`counts` matrix could not be found in `', assay, '` assay slot.'
        ),
        call. = FALSE
      )
    }
    result <- pbapply::pblapply(
      genes,
      function(x) {
        genes_here <- intersect(x, rownames(object@assays[[assay]]@counts))
        if ( length(genes_here) == 1 ) {
          object@assays[[assay]]@counts[genes_here,] /
          Matrix::colSums(object@assays[[assay]]@counts)
        } else {
          Matrix::colSums(object@assays[[assay]]@counts[genes_here,]) /
          Matrix::colSums(object@assays[[assay]]@counts)
        }
      }
    )
  }
  ##--------------------------------------------------------------------------##
  ## return list with results
  ##--------------------------------------------------------------------------##
  return(result)
}
