##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
overview_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(
    overview_projection_data_to_plot()
  )
  if (!is.null(input[['overview_projection_brush']])) {
    req(
      input[['overview_projection_brush']][['mapping']][['x']] %in%
        colnames(overview_projection_data_to_plot()[['coordinates']])
    )
  }
  selected_cells <- brushedPoints(
      cbind(
        overview_projection_data_to_plot()[['cells_df']],
        overview_projection_data_to_plot()[['coordinates']]
      ),
      input[['overview_projection_brush']]
    )
  return(selected_cells)
})
