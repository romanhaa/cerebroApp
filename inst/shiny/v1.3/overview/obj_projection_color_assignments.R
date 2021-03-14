##----------------------------------------------------------------------------##
## Color assignments.
##----------------------------------------------------------------------------##
overview_projection_color_assignments <- reactive({
  req(
    overview_projection_data(),
    overview_projection_parameters_plot()
  )
  debug_log('--> trigger "overview_projection_color_assignments"', 'v')
  colors <- assignColorsToGroups(
    overview_projection_data(),
    overview_projection_parameters_plot()[['color_variable']]
  )
  debug_log(str(colors), 'vv')
  return(colors)
})
