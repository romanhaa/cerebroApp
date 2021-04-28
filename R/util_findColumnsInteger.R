#' @title test
#' @description Functions to find columns of specific type (for automatic formatting).
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test
.findColumnsInteger <- function(df, columns_to_test) {
  columns_indices <- c()
  for ( i in columns_to_test ) {
    if (
      any(is.na(df[[i]])) == FALSE &&
      is.numeric(df[[i]]) &&
      all.equal(df[[i]], as.integer(df[[i]]), check.attributes = FALSE) == TRUE
    ) {
      columns_indices <- c(columns_indices, i)
    }
  }
  return(columns_indices)
}
