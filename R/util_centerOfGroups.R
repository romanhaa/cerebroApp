#' @title test
#' @description Function to calculate center of groups in projections/
#' trajectories.
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test
.centerOfGroups <- function(coordinates, df, n_dimensions, group) {
  ## check number of dimenions in projection
  ## ... 2 dimensions
  if ( n_dimensions == 2 ) {
    ## calculate center for groups and return
    tidyr::tibble(
      x = coordinates[[1]],
      y = coordinates[[2]],
      group = df[[ group ]]
    ) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      x_median = median(x),
      y_median = median(y),
      .groups = 'drop_last'
    ) %>%
    dplyr::ungroup() %>%
    return()
  ## ... 3 dimensions
  } else if ( n_dimensions == 3 && is.numeric(coordinates[[3]]) ) {
    ## calculate center for groups and return
    tidyr::tibble(
      x = coordinates[[1]],
      y = coordinates[[2]],
      z = coordinates[[3]],
      group = df[[ group ]]
    ) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      x_median = median(x),
      y_median = median(y),
      z_median = median(z),
      .groups = 'drop_last'
    ) %>%
    dplyr::ungroup() %>%
    return()
  }
}
