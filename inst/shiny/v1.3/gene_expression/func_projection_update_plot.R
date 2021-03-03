## function to be executed to update figure
expression_projection_update_plot <- function(input) {
  # process input parameters and give to JavaScript function to update plot
  # message('update plot')
  coordinates <- input[['coordinates']]
  reset_axes <- input[['reset_axes']]
  expression_levels <- input[['expression_levels']]
  plot_parameters <- input[['plot_parameters']]
  color_settings <- input[['color_settings']]
  hover_info <- input[['hover_info']]
  trajectory <- input[['trajectory']]
  separate_panels <- input[['separate_panels']]
  ## sort cells based on expression (if applicable)
  if (
    plot_parameters[['plot_order']]=='Highest expression on top' &&
    separate_panels == FALSE
  ) {
    cell_order <- order(expression_levels)
    coordinates <- coordinates[cell_order,]
    hover_info <- hover_info[cell_order]
    if (is.list(expression_levels)) {
      for (i in 1:length(expression_levels)) {
        expression_levels[[i]] <- expression_levels[[i]][cell_order]
      }
    } else {
      expression_levels <- expression_levels[cell_order]
    }
  }
  ## define output_data
  output_data <- list(
    x = coordinates[[1]],
    y = coordinates[[2]],
    color = expression_levels,
    point_size = plot_parameters[["point_size"]],
    point_opacity = plot_parameters[["point_opacity"]],
    point_line = list(),
    x_range = plot_parameters[["x_range"]],
    y_range = plot_parameters[["y_range"]],
    reset_axes = reset_axes
  )
  if ( plot_parameters[["draw_border"]] ) {
    output_data[['point_line']] <- list(
      color = "rgb(196,196,196)",
      width = 1
    )
  }
  ## define output_color
  output_color <- list(
    scale = color_settings[['color_scale']],
    range = color_settings[['color_range']]
  )
  ## prepare hover info
  output_hover <- list(
    hoverinfo = ifelse(plot_parameters[["hover_info"]], 'text', 'skip'),
    text = 'empty'
  )
  if ( plot_parameters[["hover_info"]] ) {
    output_hover[['text']] <- unname(hover_info)
  }
  ## process trajectory data
  trajectory_lines <- list()
  if (plot_parameters[['is_trajectory']]) {
    ## fix order of trajectory meta data if cells are sorted by expression
    if (
      plot_parameters[['plot_order']]=='Highest expression on top' &&
      separate_panels == FALSE
    ) {
      trajectory[['meta']] <- trajectory[['meta']][cell_order,]
    }
    ## add additional info to hover info
    if (plot_parameters[['hover_info']]) {
      output_hover[['text']] <- glue::glue(
        "{output_hover[['text']]}<br>",
        "<b>State</b>: {trajectory[['meta']]$state}<br>",
        "<b>Pseudotime</b>: {formatC(trajectory[['meta']]$pseudotime, format = 'f', digits = 2)}"
      )
    }
    ## convert edges of trajectory into list format to plot with plotly
    trajectory_edges <- trajectory[['edges']]
    for (i in 1:nrow(trajectory_edges) ) {
      line = list(
        type = "line",
        line = list(color = "black", width = 1),
        xref = "x",
        yref = "y",
        x0 = trajectory_edges$source_dim_1[i],
        y0 = trajectory_edges$source_dim_2[i],
        x1 = trajectory_edges$target_dim_1[i],
        y1 = trajectory_edges$target_dim_2[i]
      )
      trajectory_lines <- c(trajectory_lines, list(line))
    }
  }
  ## print details for debugging purposes
  # if (
  #   exists('mode_debugging') &&
  #   mode_debugging == TRUE &&
  #   length(hover_info) > 1
  # ) {
  #   random_cells <- c(10, 51, 79)
  #   for (i in random_cells) {
  #     current_cell <- gsub(hover_info[i], pattern = '<b>Cell</b>: ', replacement = '')
  #     current_cell <- gsub(current_cell, pattern = '<br>.*', replacement = '')
  #     current_cell <- unname(current_cell)
  #     coordinates_shown <- coordinates[i,]
  #     hover_shown <- hover_info[i]
  #     expression_shown <- expression_levels[i]
  #     position_of_current_cell_in_original_data <- which(getMetaData()$cell_barcode == current_cell)
  #     coordinates_should <- data_set()$projections[[expression_projection_parameters_plot()$projection]][position_of_current_cell_in_original_data,]
  #     expression_should <- unname(getMeanExpressionForCells(
  #       cells = c(current_cell),
  #       genes = expression_selected_genes()$genes_to_display_present
  #     ))
  #     if (is.na(expression_should)) {
  #       expression_should <- 0
  #     }
  #     message(
  #       glue::glue(
  #         '{current_cell}: ',
  #         'coords. {round(coordinates_shown[1], digits=2)}/{round(coordinates_should[1], digits=2)} // ',
  #         '{round(coordinates_shown[2], digits=2)}/{round(coordinates_should[2], digits=2)}, ',
  #         'expr. {round(expression_shown, digits=2)}/{round(expression_should, digits=2)}'
  #       )
  #     )
  #   }
  # }
  ## call JavaScript functions to update plot
  if (
    plot_parameters[['n_dimensions']] == 2 &&
    is.list(input[['expression_levels']]) == FALSE
  ) {
    shinyjs::js$expressionProjectionUpdatePlot2D(
      output_data,
      output_hover,
      output_color,
      trajectory_lines
    )
  } else if (
    plot_parameters[['n_dimensions']] == 2 &&
    separate_panels == TRUE &&
    is.list(input[['expression_levels']]) == TRUE
  ) {
    shinyjs::js$expressionProjectionUpdatePlot2DMultiPanel(
      output_data,
      output_hover,
      output_color,
      trajectory_lines
    )
  } else if ( plot_parameters[['n_dimensions']] == 3 ) {
    output_data[['z']] <- coordinates[[3]]
    shinyjs::js$expressionProjectionUpdatePlot3D(
      output_data,
      output_hover,
      output_color
    )
  }
}
