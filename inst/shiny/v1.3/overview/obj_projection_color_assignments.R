##----------------------------------------------------------------------------##
## Color assignments.
##----------------------------------------------------------------------------##
overview_projection_color_assignments <- reactive({
  req(
    overview_projection_data(),
    overview_projection_parameters_plot()
  )
  # message('--> trigger "overview_projection_color_assignments"')
  colors <- assignColorsToGroups(
    overview_projection_data(),
    overview_projection_parameters_plot()[['color_variable']]
  )
  # message(str(colors))
  return(colors)
})
