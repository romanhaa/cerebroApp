##----------------------------------------------------------------------------##
## Function to plot expression in single panel in 2D (for export).
##----------------------------------------------------------------------------##
pltExpProj2DSglPanExp <- function(
  df,
  point_size,
  point_opacity,
  point_border,
  color_scale,
  color_range,
  x_range,
  y_range
) {
  ##
  if ( point_border == TRUE ) {
    stroke <- 0.2
  } else {
    stroke <- 0
  }
  ## prepare plot
  plot <- ggplot(
      df,
      aes_q(
        x = as.name(colnames(df)[1]),
        y = as.name(colnames(df)[2]),
        fill = as.name("level")
      )
    ) +
    geom_point(
      shape = 21,
      size = point_size/3,
      stroke = stroke,
      color = "#c4c4c4",
      alpha = point_opacity
    ) +
    lims(x = x_range, y = y_range) +
    theme_bw()
  ## check if selected color scale
  ## ... selected color scale is "Viridis"
  if ( tolower(color_scale) == 'viridis' ) {
    ## add color scale to plot
    plot <- plot +
      viridis::scale_fill_viridis(
        option = "viridis",
        limits = color_range,
        oob = scales::squish,
        direction = -1,
        name = "Log-normalised\nexpression",
        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
      )
  ## ... selected color scale is anything else than "Viridis"
  } else {
    ## add color scale to plot
    plot <- plot +
      scale_fill_distiller(
        palette = color_scale,
        limits = color_range,
        oob = scales::squish,
        direction = 1,
        name = "Log-normalised\nexpression",
        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
      )
  }
  return(plot)
}