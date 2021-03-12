##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
overview_projection_data_to_plot_raw <- reactive({
  req(
    overview_projection_data(),
    overview_projection_coordinates(),
    overview_projection_parameters_plot(),
    reactive_colors(),
    overview_projection_hover_info(),
    nrow(overview_projection_data()) == length(overview_projection_hover_info()) || overview_projection_hover_info() == "none"
  )
  # message('--> trigger "overview_projection_data_to_plot"')
  ## get colors for groups (if applicable)
  if ( is.numeric(overview_projection_parameters_plot()[['color_variable']]) ) {
    color_assignments <- NA
  } else {
    color_assignments <- assignColorsToGroups(
      overview_projection_data(),
      overview_projection_parameters_plot()[['color_variable']]
    )
  }
  ## print details for debugging purposes
  # if (
  #   exists('mode_debugging') &&
  #   mode_debugging == TRUE &&
  #   length(overview_projection_hover_info()) > 1
  # ) {
  #   random_cells <- c(10, 51, 79)
  #   for (i in random_cells) {
  #     current_cell <- overview_projection_data()$cell_barcode[i]
  #     coordinates_shown <- overview_projection_coordinates()[i,]
  #     hover_shown <- overview_projection_hover_info()[i]
  #     position_of_current_cell_in_original_data <- which(getMetaData()$cell_barcode == current_cell)
  #     coordinates_should <- data_set()$projections[[overview_projection_parameters_plot()$projection]][position_of_current_cell_in_original_data,]
  #     message(
  #       glue::glue(
  #         '{current_cell}: ',
  #         'coords. {round(coordinates_shown[1], digits=2)}/{round(coordinates_should[1], digits=2)} // ',
  #         '{round(coordinates_shown[2], digits=2)}/{round(coordinates_should[2], digits=2)}'
  #       )
  #     )
  #   }
  # }
  ## return collect data
  to_return <- list(
    cells_df = overview_projection_data(),
    coordinates = overview_projection_coordinates(),
    reset_axes = isolate(overview_projection_parameters_other[['reset_axes']]),
    plot_parameters = overview_projection_parameters_plot(),
    color_assignments = color_assignments,
    hover_info = overview_projection_hover_info()
  )
  # message(str(to_return))
  return(to_return)
})

overview_projection_data_to_plot <- debounce(overview_projection_data_to_plot_raw, 150)
