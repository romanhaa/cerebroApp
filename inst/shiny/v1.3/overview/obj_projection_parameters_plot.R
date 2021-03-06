##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
overview_projection_parameters_plot_raw <- reactive({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_to_display"]] %in% availableProjections(),
    input[["overview_projection_point_color"]],
    input[["overview_projection_point_color"]] %in% colnames(getMetaData()),
    input[["overview_projection_point_size"]],
    input[["overview_projection_point_opacity"]],
    !is.null(input[["overview_projection_show_group_label"]])
  )
  if (exists('mode_debugging') && grepl('v', mode_debugging, ignore.case=TRUE)) {
    message('--> trigger "overview_projection_parameters_plot"')
  }
  parameters <- list(
    projection = input[["overview_projection_to_display"]],
    n_dimensions = ncol(getProjection(input[["overview_projection_to_display"]])),
    color_variable = input[["overview_projection_point_color"]],
    point_size = input[["overview_projection_point_size"]],
    point_opacity = input[["overview_projection_point_opacity"]],
    group_labels = input[["overview_projection_show_group_label"]]
  )
  if (exists('mode_debugging') && grepl('vv', mode_debugging, ignore.case=TRUE)) {
    message(str(parameters))
  }
  return(parameters)
})

overview_projection_parameters_plot <- debounce(overview_projection_parameters_plot_raw, 150)
