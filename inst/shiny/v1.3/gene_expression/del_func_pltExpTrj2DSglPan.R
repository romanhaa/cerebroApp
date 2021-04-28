##----------------------------------------------------------------------------##
## Function to plot expression in trajectory.
##----------------------------------------------------------------------------##

pltExpTrj2DSglPan <- function(
  df,
  trajectory_edges,
  point_size,
  point_opacity,
  draw_border,
  plot_order,
  color_scale,
  color_range,
  x_range,
  y_range,
  show_hover_info
) {

  ##
  if ( draw_border == TRUE ) {
    point_border <- list(
      color = "rgb(196,196,196)",
      width = 1
    )
  } else {
    point_border <- NULL
  }

  ## bring cells in order, either random or highest expression on top
  df <- setRowOrder(df, plot_order)

  ## prepare hover info
  if ( show_hover_info == TRUE ) {
    hover_info <- buildHoverInfoForProjections(df)
    hover_info <- glue::glue(
      "{hover_info}
      <b>State</b>: {df()$state}
      <b>Pseudotime</b>: {formatC(df()$pseudotime, format = 'f', digits = 2)}"
    )
    parameter_hoverinfo <- "text"
    parameter_text <- ~hover_info
  } else {
    hover_info <- NULL
    parameter_hoverinfo <- "skip"
    parameter_text <- NULL
  }

  ## convert edges of trajectory into list format to plot with plotly
  trajectory_lines <- list()
  for (i in 1:nrow(trajectory_edges) ) {
    line = list(
      type = "line",
      line = list(color = "black"),
      xref = "x",
      yref = "y",
      x0 = trajectory_edges$source_dim_1[i],
      y0 = trajectory_edges$source_dim_2[i],
      x1 = trajectory_edges$target_dim_1[i],
      y1 = trajectory_edges$target_dim_2[i]
    )
    trajectory_lines <- c(trajectory_lines, list(line))
  }

  ##
  plotly::plot_ly(
    data = df,
    x = ~DR_1,
    y = ~DR_2,
    type = "scatter",
    mode = "markers",
    marker = list(
      colorbar = list(
        title = "Expression",
        ticks = 'outside',
        outlinewidth = 1,
        outlinecolor = 'black'
      ),
      color = ~level,
      opacity = point_opacity,
      colorscale = color_scale,
      cauto = FALSE,
      cmin = color_range[1],
      cmax = color_range[2],
      reversescale = TRUE,
      line = point_border,
      size = point_size
    ),
    hoverinfo = parameter_hoverinfo,
    text = parameter_text,
    source = "expression_projection"
  ) %>%
  plotly::layout(
    shapes = trajectory_lines,
    xaxis = list(
      mirror = TRUE,
      showline = TRUE,
      zeroline = FALSE,
      range = x_range
    ),
    yaxis = list(
      mirror = TRUE,
      showline = TRUE,
      zeroline = FALSE,
      range = y_range
    ),
    hoverlabel = list(
      font = list(
        size = 11,
        color = "black"
      ),
      bgcolor = "lightgrey",
      align = 'left'
    )
  ) %>%
  return()
}