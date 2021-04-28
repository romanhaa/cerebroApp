#' @title test
#' @description Functions to find columns of specific type (for automatic formatting).
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test
.findColumnsPercentage <- function(df) {
  columns_indices <- c()
  for ( i in 1:ncol(df) ) {
    if (
      grepl(colnames(df)[i], pattern = "pct|percent|%", ignore.case = TRUE) &&
      any(is.na(df[[i]])) == FALSE &&
      is.numeric(df[[i]]) &&
      min(df[[i]], na.rm = TRUE) >= 0 &&
      max(df[[i]], na.rm = TRUE) <= 100
    ) {
      columns_indices <- c(columns_indices, i)
    }
  }
  return(columns_indices)
}
