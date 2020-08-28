#' @title
#' Calculate percentage of transcripts of gene list.
#'
#' @description
#' Get percentage of transcripts of gene list compared to all transcripts per
#' cell.
#'
#' @param object Seurat object.
#' @param assay Assay to pull counts from; defaults to 'RNA'. Only relevant in
#' Seurat v3.0 or higher since the concept of assays wasn't implemented before.
#' @param genes List(s) of genes.
#'
#' @return
#' List of lists containing the percentages of expression for each provided
#' gene list.
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.3/pbmc_seurat.rds",
#'   package = "cerebroApp"))
#' pbmc <- calculatePercentGenes(
#'   object = pbmc,
#'   assay = 'RNA',
#'   genes = list('example' = c('FCN1','CD3D'))
#' )
#'
#' @importFrom Matrix colSums
#' @importFrom pbapply pblapply
#'
#' @export
#'
calculatePercentGenes <- function(
  object,
  assay = 'RNA',
  genes
) {

  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  ## check if Seurat is installed
  if ( !requireNamespace("Seurat", quietly = TRUE) ) {
    stop(
      "The 'Seurat' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## check that Seurat package is at least v3.0
  if ( utils::packageVersion('Seurat') < 3 ) {
    stop(
      paste0(
        "The installed Seurat package is of version `", utils::packageVersion('Seurat'),
        "`, but at least v3.0 is required."
      ),
      call. = FALSE
    )
  }

  ## check if provided object is of class "Seurat"
  if ( class(object) != "Seurat" ) {
    stop(
      paste0(
        "Provided object is of class `", class(object), "` but must be of class 'Seurat'."
      ),
      call. = FALSE
    )
  }

  ## check version of Seurat object and stop if it is lower than 3
  if ( object@version < 3 ) {
    stop(
      paste0(
        "Provided Seurat object has version `", object@version, "` but must be at least 3.0."
      ),
      call. = FALSE
    )
  }

  ## check if provided assay exists
  if ( assay %in% names(object@assays) == FALSE ) {
    stop(
      paste0(
        'Specified assay slot `', assay, '` could not be found in provided Seurat object.'
      ),
      call. = FALSE
    )
  }

  ## check if `counts` matrix exist in provided assay
  if ( is.null(object@assays[[assay]]@counts) ) {
    stop(
      paste0(
        '`counts` matrix could not be found in `', assay, '` assay slot of the provided Seurat object.'
      ),
      call. = FALSE
    )
  }

  ##--------------------------------------------------------------------------##
  ## get for every supplied gene list, get the genes that are present in the
  ## data set and calculate the percentage of transcripts that they account for
  ##--------------------------------------------------------------------------##

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

  ##--------------------------------------------------------------------------##
  ## return list with results
  ##--------------------------------------------------------------------------##
  return(result)
}
