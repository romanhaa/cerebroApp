##----------------------------------------------------------------------------##
## Function to plot expression in single panel in 3D.
##----------------------------------------------------------------------------##

pltExpProj3DSglPan <- function(
  df,
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
      <b>Expression level</b>: {formatC(df$level, format = 'f', digits = 3)}"
    )
    parameter_hoverinfo <- "text"
    parameter_text <- ~hover_info
  } else {
    hover_info <- NULL
    parameter_hoverinfo <- "skip"
    parameter_text <- NULL
  }

  plotly::plot_ly(
    df,
    x = df[,1],
    y = df[,2],
    z = df[,3],
    type = "scatter3d",
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
    scene = list(
      xaxis = list(
        title = colnames(df)[1],
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = colnames(df)[2],
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE
      ),
      zaxis = list(
        title = colnames(df)[3],
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE
      )
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