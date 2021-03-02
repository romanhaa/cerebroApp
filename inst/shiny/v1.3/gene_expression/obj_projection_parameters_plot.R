##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
expression_projection_parameters_plot_raw <- reactive({
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_plotting_order"]],
    input[["expression_projection_point_size"]],
    input[["expression_projection_point_opacity"]],
    !is.null(input[["expression_projection_point_border"]]),
    input[["expression_projection_scale_x_manual_range"]],
    input[["expression_projection_scale_y_manual_range"]],
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]]),
    input[["expression_projection_to_display"]] %in% availableProjections() ||
    input[["expression_projection_to_display"]] %in% available_trajectories()
  )
  # message('--> trigger "expression_projection_parameters_plot_raw"')
  if ( input[["expression_projection_to_display"]] %in% availableProjections() ) {
    is_trajectory = FALSE
    n_dimensions = ncol(getProjection(input[["expression_projection_to_display"]]))
  } else {
    is_trajectory = TRUE
    # currently, only trajectories with 2 dimensions are supported
    n_dimensions = 2
  }
  parameters <- list(
    projection = input[["expression_projection_to_display"]],
    plot_order = input[["expression_projection_plotting_order"]],
    n_dimensions = n_dimensions,
    is_trajectory = is_trajectory,
    point_size = input[["expression_projection_point_size"]],
    point_opacity = input[["expression_projection_point_opacity"]],
    draw_border = input[["expression_projection_point_border"]],
    x_range = input[["expression_projection_scale_x_manual_range"]],
    y_range = input[["expression_projection_scale_y_manual_range"]],
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  # message(str(parameters))
  return(parameters)
})

expression_projection_parameters_plot <- debounce(expression_projection_parameters_plot_raw, 150)

##
expression_projection_parameters_other <- reactiveValues(
  reset_axes = FALSE
)

##
observeEvent(input[['expression_projection_to_display']], {
  # message('--> set "gene expression: reset_axes"')
  expression_projection_parameters_other[['reset_axes']] <- TRUE
})
