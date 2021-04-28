#' @title test
#' @description test
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test
.fetchExpressionFromH5File <- function(matrix, genes=c()) {
  genes_present <- genes[which(genes %in% rownames(matrix))]
  if (length(genes_present)==0) {
    return(rep(0, ncol(matrix)))
  } else if (length(genes_present)==1) {
    values <- unname(matrix[genes_present,])
  } else {
    values <- unname(Matrix::colMeans(matrix[genes_present,]))
  }
  return(values)
}
