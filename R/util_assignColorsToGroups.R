#' @title Assign colors to groups.
#' @description
#' Takes a data frame, column name, and list of grouping variables with already
#' assigned colors to the group levels. If the provided grouping variable
#' already has color assignments, return a named vector of the colors and the
#' corresponding group levels. Otherwise, it will assign new colors from the
#' default color set. Only works for categorical values, not numerical values.
#' @param df Data frame.
#' @param grouping_variable Name of column that contains values to which colors
#' should be assigned.
#' @param assigned_colors List of named vectors for groups with already assigned
#' colors.
#' @return The return value is a named vector with color assigned to each group
#' level.
.assignColorsToGroups <- function(df, grouping_variable, assigned_colors) {
  ## check if colors are already assigned in assigned_colors
  ## ... already assigned
  if ( grouping_variable %in% names(assigned_colors) ) {
    ## take colors from assigned_colors
    colors_for_groups <- assigned_colors[[ grouping_variable ]]
  ## ... not assigned but values are either factors or characters
  } else if (
    is.factor(df[[ grouping_variable ]]) ||
    is.character(df[[ grouping_variable ]])
  ) {
    ## check type of values
    ## ... factors
    if ( is.factor(df[[ grouping_variable ]]) ) {
      ## get factor levels and assign colors
      colors_for_groups <- setNames(
        default_colorset[seq_along(levels(df[[ grouping_variable ]]))],
        levels(df[[ grouping_variable ]])
      )
    ## ... characters
    } else if ( is.character(df[[ grouping_variable ]]) ) {
      ## get unique values and assign colors
      colors_for_groups <- setNames(
        default_colorset[seq_along(unique(df[[ grouping_variable ]]))],
        unique(df[[ grouping_variable ]])
      )
    }
  ## ... none of the above (e.g. numeric values)
  } else {
    colors_for_groups <- NULL
  }
  return(colors_for_groups)
}
