##----------------------------------------------------------------------------##
## Color assignments.
##----------------------------------------------------------------------------##
overview_projection_color_assignments <- reactive({
  req(
    overview_projection_data(),
    overview_projection_parameters_plot()
  )
  if (exists('mode_debugging') && grepl('v', mode_debugging, ignore.case=TRUE)) {
    message('--> trigger "overview_projection_color_assignments"')
  }
  colors <- assignColorsToGroups(
    overview_projection_data(),
    overview_projection_parameters_plot()[['color_variable']]
  )
  if (exists('mode_debugging') && grepl('vv', mode_debugging, ignore.case=TRUE)) {
    message(str(colors))
  }
  return(colors)
})
