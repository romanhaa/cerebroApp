##----------------------------------------------------------------------------##
## Coordinates of cells in projection.
##----------------------------------------------------------------------------##
overview_projection_coordinates <- reactive({
  req(
    overview_projection_parameters_plot(),
    overview_projection_cells_to_show()
  )
  debug_log('--> trigger "overview_projection_coordinates"', 'v')
  parameters <- overview_projection_parameters_plot()
  cells_to_show <- overview_projection_cells_to_show()
  req(parameters[["projection"]] %in% availableProjections())
  coordinates <- getProjection(parameters[["projection"]])[cells_to_show,]
  debug_log(str(coordinates), 'vv')
  return(coordinates)
})
