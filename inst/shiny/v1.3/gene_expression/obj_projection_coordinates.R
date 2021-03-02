##----------------------------------------------------------------------------##
## Coordinates of cells in projection.
##----------------------------------------------------------------------------##
expression_projection_coordinates <- reactive({
  req(
    expression_projection_parameters_plot(),
    expression_projection_cells_to_show()
  )
  # message('--> trigger "expression_projection_coordinates"')
  parameters <- expression_projection_parameters_plot()
  cells_to_show <- expression_projection_cells_to_show()
  req(
    parameters[["projection"]] %in% availableProjections() ||
    parameters[["projection"]] %in% available_trajectories()
  )
  if ( parameters[["projection"]] %in% availableProjections() ) {
    coordinates <- getProjection(parameters[["projection"]])[cells_to_show,]
  } else if ( parameters[["projection"]] %in% available_trajectories() ) {
    selection <- strsplit(parameters[["projection"]], split = ' // ')[[1]]
    req(
      selection[1] %in% getMethodsForTrajectories(),
      selection[2] %in% getNamesOfTrajectories(selection[1])
    )
    coordinates <- getTrajectory(selection[1], selection[2])[['meta']][cells_to_show,c(1,2)]
  }
#   message(str(coordinates))
  return(coordinates)
})
