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
    !is.null(input[["overview_projection_point_border"]]),
    input[["overview_projection_scale_x_manual_range"]],
    input[["overview_projection_scale_y_manual_range"]],
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  # message('--> trigger "overview_projection_parameters_plot"')
  parameters <- list(
    projection = input[["overview_projection_to_display"]],
    n_dimensions = ncol(getProjection(input[["overview_projection_to_display"]])),
    color_variable = input[["overview_projection_point_color"]],
    point_size = input[["overview_projection_point_size"]],
    point_opacity = input[["overview_projection_point_opacity"]],
    draw_border = input[["overview_projection_point_border"]],
    group_labels = input[["overview_projection_show_group_label"]],
    x_range = input[["overview_projection_scale_x_manual_range"]],
    y_range = input[["overview_projection_scale_y_manual_range"]],
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  # message(str(parameters))
  return(parameters)
})

overview_projection_parameters_plot <- debounce(overview_projection_parameters_plot_raw, 150)

##
overview_projection_parameters_other <- reactiveValues(
  reset_axes = FALSE
)

##
observeEvent(input[['overview_projection_to_display']], {
  # message('--> set "overview: reset_axes"')
  overview_projection_parameters_other[['reset_axes']] <- TRUE
})
