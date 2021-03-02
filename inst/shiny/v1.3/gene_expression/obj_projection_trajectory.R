##----------------------------------------------------------------------------##
## Data to draw trajectory in projection.
##----------------------------------------------------------------------------##
expression_projection_trajectory <- reactive({
  req(
    expression_projection_parameters_plot(),
    expression_projection_cells_to_show()
  )
  # message('--> trigger "expression_projection_trajectory"')
  parameters <- expression_projection_parameters_plot()
  cells_to_show <- expression_projection_cells_to_show()
  if ( parameters[["projection"]] %in% availableProjections()) {
    trajectory_data <- list()
  } else {
    ## split selection into method and name
    selection <- strsplit(parameters[["projection"]], split = ' // ')[[1]]
    ## check if method and name exist and don't proceed if not
    req(
      selection[1] %in% getMethodsForTrajectories(),
      selection[2] %in% getNamesOfTrajectories(selection[1])
    )
    ## collect trajectory data
    trajectory_data <- getTrajectory(
      selection[1],
      selection[2]
    )
    trajectory_data[['meta']] <- trajectory_data[['meta']][cells_to_show,]
  }
#   message(str(trajectory_data))
  return(trajectory_data)
})
