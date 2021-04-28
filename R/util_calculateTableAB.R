#' @title test
#' @description Function to calculate A-by-B tables, e.g. samples by clusters.
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test
.calculateTableAB <- function(table, groupA, groupB, mode, percent) {
  ## TODO: more safety checks?
  ## check if specified group columns exist in table
  if ( groupA %in% colnames(table) == FALSE ) {
    stop(
      glue::glue(
        "Column specified as groupA (`{groupA}`) could not be found in meta ",
        "data."
      ),
      call. = FALSE
    )
  }
  if ( groupB %in% colnames(table) == FALSE ) {
    stop(
      glue::glue(
        "Column specified as groupB (`{groupB}`) could not be found in meta ",
        "data."
      ),
      call. = FALSE
    )
  }
  ## subset columns
  table <- table[,c(groupA, groupB)]
  ## factorize group columns A if not already a factor
  if ( is.character(table[[groupA]]) ) {
    levels_groupA <- table[[groupA]] %>% unique() %>% sort()
    table[,groupA] <- factor(table[[groupA]], levels = levels_groupA, exclude = NULL)
  } else {
    levels_groupA <- levels(table[,groupA])
  }
  ## factorize group columns B if not already a factor
  if ( is.character(table[[groupB]]) ) {
    levels_groupB <- table[[groupB]] %>% unique() %>% sort()
    table[,groupB] <- factor(table[[groupB]], levels = levels_groupB, exclude = NULL)
  } else {
    levels_groupB <- levels(table[,groupB])
  }
  ## prepare table in long format
  table <- table %>%
    dplyr::arrange(dplyr::across(c(groupA, groupB))) %>%
    dplyr::group_by(dplyr::across(c(groupA, groupB))) %>%
    dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
    dplyr::group_by(dplyr::across(c(groupA))) %>%
    dplyr::mutate(total_cell_count = sum(count)) %>%
    dplyr::ungroup()
  ## convert counts to percent
  if ( percent == TRUE ) {
    table <- table %>%
      dplyr::mutate(count = count / total_cell_count) %>%
      dplyr::select(
        tidyselect::all_of(c(groupA, "total_cell_count", groupB, "count"))
      )
  }
  ## bring table into wide format
  if ( mode == "wide" ) {
    table <- table %>%
      tidyr::pivot_wider(
        id_cols = tidyselect::all_of(c(groupA, "total_cell_count")),
        names_from = tidyselect::all_of(groupB),
        values_from = "count",
        values_fill = 0
      ) %>%
      dplyr::select(
        tidyselect::all_of(groupA), 'total_cell_count',
        tidyselect::any_of(levels_groupB)
      )
    ## fix order of columns if cell cycle info was chosen as second group
    if (
      'G1' %in% colnames(table) &&
      'G2M' %in% colnames(table) &&
      'S' %in% colnames(table)
    ) {
      table <- table %>%
        dplyr::select(
          tidyselect::all_of(c(groupA, 'total_cell_count', 'G1', 'S', 'G2M')),
          dplyr::everything()
        )
    }
  }
  return(table)
}
