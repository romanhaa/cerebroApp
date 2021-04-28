#' @title test
#' @description Randomly subset cells in data frame, if necessary.
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test
.randomlySubsetCells <- function(df, percentage) {
  ## check if subsetting is necessary
  ## ... percentage is less than 100
  if ( percentage < 100 ) {
    ## calculate how many cells should be left after subsetting
    size_of_subset <- ceiling(percentage / 100 * nrow(df))
    ## get IDs of all cells
    cell_ids <- rownames(df)
    ## subset cell IDs
    subset_of_cell_ids <- cell_ids[ sample(seq_along(cell_ids), size_of_subset) ]
    ## subset table and return
    return(df[subset_of_cell_ids,])
  ## ... percentage is 100 -> no subsetting needed
  } else {
    ## return original table
    return(df)
  }
}
