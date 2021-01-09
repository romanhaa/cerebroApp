##----------------------------------------------------------------------------##
## Input parameters for filtering cells.
##----------------------------------------------------------------------------##
overview_projection_parameters_cell_filtering_raw <- reactive({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_percentage_cells_to_show"]]
  )
  ## require group filters UI elements and at least 1 group level to be selected
  for ( i in getGroups() ) {
    req(input[[paste0("overview_projection_group_filter_", i)]])
  }
  parameters <- list(
    projection = input[["overview_projection_to_display"]],
    pct_cells = input[["overview_projection_percentage_cells_to_show"]],
    group_filters = list()
  )
  ## store group filters
  for ( i in getGroups() ) {
    parameters[['group_filters']][[ i ]] <- input[[paste0("overview_projection_group_filter_", i)]]
  }
  return(parameters)
})

overview_projection_parameters_cell_filtering <- debounce(overview_projection_parameters_cell_filtering_raw, 150)
