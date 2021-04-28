#' @title test
#' @description test
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test
.checkMatrixFormat <- function(matrix) {
  accepted_formats <- c('matrix', 'Matrix', 'dgCMatrix', 'DelayedMatrix')
  if ( any(class(matrix) %in% accepted_formats) == FALSE ) {
    stop(
      "provided object is not in one of the accepted matrix formats",
      call. = FALSE
    )
  } else {
    return(TRUE)
  }
}
