##----------------------------------------------------------------------------##
## Color assignments.
##----------------------------------------------------------------------------##
overview_projection_color_assignments <- reactive({
  req(
    overview_projection_data(),
    overview_projection_parameters_plot()
  )
  return(
    assignColorsToGroups(
      overview_projection_data(),
      overview_projection_parameters_plot()[['color_variable']]
    )
  )
})
