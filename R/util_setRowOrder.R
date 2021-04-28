#' @title test
#' @description Set order of rows in data frame.
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test

.setRowOrder <- function(df, order) {
  if ( order == 'Random' ) {
    return(df[ sample(1:nrow(df)) , ])
  } else if ( order == "Highest expression on top" ) {
    return(dplyr::arrange(df, level))
  } else {
    return(df)
  }
}
