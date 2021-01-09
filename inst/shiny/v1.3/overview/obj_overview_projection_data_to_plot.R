##
overview_projection_data_to_plot <- reactive({
  req(
    overview_projection_data(),
    overview_projection_parameters_plot(),
    reactive_colors(),
    overview_projection_hover_info()
  )
  if ( is.numeric(overview_projection_parameters_plot()[['color_variable']]) ) {
    color_assignments <- NA
  } else {
    color_assignments <- assignColorsToGroups(
      overview_projection_data(),
      overview_projection_parameters_plot()[['color_variable']]
    )
  }
  list(
    cells_df = overview_projection_data(),
    plot_parameters = overview_projection_parameters_plot(),
    color_assignments = color_assignments,
    hover_info = overview_projection_hover_info()
  )
})
