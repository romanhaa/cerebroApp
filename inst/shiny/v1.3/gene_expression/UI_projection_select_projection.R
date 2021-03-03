##----------------------------------------------------------------------------##
## UI elements to choose which projection/trajectory to show.
##----------------------------------------------------------------------------##
output[["expression_projection_select_projection_UI"]] <- renderUI({
  available_projections <- availableProjections()
  available_trajectories <- available_trajectories()
  selectInput(
    "expression_projection_to_display",
    label = "Projection",
    choices = list(
      "Projections" = as.list(available_projections),
      "Trajectories" = as.list(available_trajectories)
    )
  )
})
