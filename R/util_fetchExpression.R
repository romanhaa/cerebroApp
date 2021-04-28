#' @title test
#' @description test
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test
.fetchExpression <- function(
  matrix = NULL,
  API = NULL,
  genes = NULL
) {
  if (!missing(matrix) && missing(API) ) {
    .fetchExpressionFromMatrix(matrix, genes)
  } else if (missing(matrix) && !missing(API)) {
    .fetchExpressionFromAPI(API, genes)
  } else {
    return(NULL)
  }
}
