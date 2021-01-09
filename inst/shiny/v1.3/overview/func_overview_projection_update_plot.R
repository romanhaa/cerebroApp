## function to be executed to update figure
overview_projection_update_plot <- function(input) {
  cells_df <- input[['cells_df']]
  plot_parameters <- input[['plot_parameters']]
  color_assignments <- input[['color_assignments']]
  hover_info <- input[['hover_info']]
  color_input <- cells_df[[ plot_parameters[['color_variable']] ]]
  if ( is.numeric(color_input) ) {
    output_meta <- list(
      color_type = 'continuous',
      traces = plot_parameters[['color_variable']],
      color_variable = plot_parameters[['color_variable']]
    )
    output_data <- list(
      x = cells_df[[1]],
      y = cells_df[[2]],
      color = color_input,
      point_size = plot_parameters[["point_size"]],
      point_opacity = plot_parameters[["point_opacity"]],
      point_line = list(),
      x_range = plot_parameters[["x_range"]],
      y_range = plot_parameters[["y_range"]]
    )
    if ( plot_parameters[["draw_border"]] ) {
      output_data[['point_line']] <- list(
        color = "rgb(196,196,196)",
        width = 1
      )
    }
    output_hover <- list(
      hoverinfo = ifelse(plot_parameters[["hover_info"]], 'text', 'skip'),
      text = 'empty'
    )
    if ( plot_parameters[["hover_info"]] ) {
      output_hover[['text']] <- unname(hover_info)
    }
    if ( plot_parameters[['n_dimensions']] == 2 ) {
      shinyjs::js$updatePlot2DContinuous(
        output_meta,
        output_data,
        output_hover
      )
    } else if ( plot_parameters[['n_dimensions']] == 3 ) {
      output_data[['z']] <- cells_df[[3]]
      shinyjs::js$updatePlot3DContinuous(
        output_meta,
        output_data,
        output_hover
      )
    }
  } else {
    output_meta <- list(
      color_type = 'categorical',
      traces = list(),
      color_variable = plot_parameters[['color_variable']]
    )
    output_data <- list(
      x = list(),
      y = list(),
      z = list(),
      color = list(),
      point_size = plot_parameters[["point_size"]],
      point_opacity = plot_parameters[["point_opacity"]],
      point_line = list(),
      x_range = plot_parameters[["x_range"]],
      y_range = plot_parameters[["y_range"]]
    )
    if ( plot_parameters[["draw_border"]] ) {
      output_data[['point_line']] <- list(
        color = "rgb(196,196,196)",
        width = 1
      )
    }
    output_hover <- list(
      hoverinfo = ifelse(plot_parameters[["hover_info"]], 'text', 'skip'),
      text = ifelse(plot_parameters[["hover_info"]], list(), 'test')
    )
    if ( plot_parameters[['n_dimensions']] == 2 ) {
      i <- 1
      for ( j in names(color_assignments) ) {
        output_meta[['traces']][[i]] <- j
        cells_to_extract <- which(color_input==j)
        output_data[['x']][[i]] <- cells_df[[1]][cells_to_extract]
        output_data[['y']][[i]] <- cells_df[[2]][cells_to_extract]
        output_data[['color']][[i]] <- unname(color_assignments[which(names(color_assignments)==j)])
        if ( plot_parameters[["hover_info"]] ) {
          hover_info_matched <- match(
            cells_df[['cell_barcode']][cells_to_extract],
            names(hover_info)
          )
          output_hover[['text']][[i]] <- unname(hover_info[hover_info_matched])
        }
        i <- i + 1
      }
      group_centers_df <- centerOfGroups(cells_df, 2, plot_parameters[['color_variable']])
      output_group_centers <- list(
        group = group_centers_df[['group']],
        x = group_centers_df[['x_median']],
        y = group_centers_df[['y_median']]
      )
      shinyjs::js$updatePlot2DCategorical(
        output_meta,
        output_data,
        output_hover,
        output_group_centers
      )
    } else if ( plot_parameters[['n_dimensions']] == 3 ) {
      i <- 1
      for ( j in names(color_assignments) ) {
        output_meta[['traces']][[i]] <- j
        cells_to_extract <- which(color_input==j)
        output_data[['x']][[i]] <- cells_df[[1]][cells_to_extract]
        output_data[['y']][[i]] <- cells_df[[2]][cells_to_extract]
        output_data[['z']][[i]] <- cells_df[[3]][cells_to_extract]
        output_data[['color']][[i]] <- unname(color_assignments[which(names(color_assignments)==j)])
        if ( plot_parameters[["hover_info"]] ) {
          hover_info_matched <- match(
            cells_df[['cell_barcode']][cells_to_extract],
            names(hover_info)
          )
          output_hover[['text']][[i]] <- unname(hover_info[hover_info_matched])
        }
        i <- i + 1
      }
      group_centers_df <- centerOfGroups(cells_df, 3, plot_parameters[['color_variable']])
      output_group_centers <- list(
        group = group_centers_df[['group']],
        x = group_centers_df[['x_median']],
        y = group_centers_df[['y_median']],
        z = group_centers_df[['z_median']]
      )
      shinyjs::js$updatePlot3DCategorical(
        output_meta,
        output_data,
        output_hover,
        output_group_centers
      )
    }
  }
}
