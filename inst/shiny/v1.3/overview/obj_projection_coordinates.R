##----------------------------------------------------------------------------##
## Coordinates of cells in projection.
##----------------------------------------------------------------------------##
overview_projection_coordinates <- reactive({
  req(
    overview_projection_parameters_plot(),
    overview_projection_cells_to_show()
  )
  if (exists('mode_debugging') && grepl('v', mode_debugging, ignore.case=TRUE)) {
    message('--> trigger "overview_projection_coordinates"')
  }
  parameters <- overview_projection_parameters_plot()
  cells_to_show <- overview_projection_cells_to_show()
  req(parameters[["projection"]] %in% availableProjections())
  coordinates <- getProjection(parameters[["projection"]])[cells_to_show,]
  if (exists('mode_debugging') && grepl('vv', mode_debugging, ignore.case=TRUE)) {
    message(str(coordinates))
  }
  return(coordinates)
})
