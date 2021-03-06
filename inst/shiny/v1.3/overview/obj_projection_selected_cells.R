##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
overview_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(overview_projection_data_to_plot())
  if (exists('mode_debugging') && grepl('v', mode_debugging, ignore.case=TRUE)) {
    message('--> trigger "overview_projection_selected_cells"')
  }
  if (!is.null(input[['overview_projection_brush']])) {
    req(
      input[['overview_projection_brush']][['mapping']][['x']] %in%
        colnames(overview_projection_data_to_plot()[['coordinates']])
    )
  }
  selected_cells <- brushedPoints(
      cbind(
        overview_projection_data_to_plot()[['coordinates']],
        overview_projection_data_to_plot()[['cells_df']]
      ),
      input[['overview_projection_brush']]
    ) %>%
    dplyr::select(3:ncol(.))
  if (exists('mode_debugging') && grepl('vv', mode_debugging, ignore.case=TRUE)) {
    message(str(selected_cells))
  }
  return(selected_cells)
})
