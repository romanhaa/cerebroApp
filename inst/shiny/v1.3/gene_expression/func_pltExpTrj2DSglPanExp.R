##----------------------------------------------------------------------------##
## Function to plot expression in trajectory (for export).
##----------------------------------------------------------------------------##
pltExpTrj2DSglPanExp <- function(
  df,
  trajectory_edges,
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
  ## start building the plot
  plot <- ggplot() +
    geom_point(
      data = df,
      aes_string(
        x = colnames(df)[1],
        y = colnames(df)[2],
        fill = as.name("level")
      ),
      shape = 21,
      size = point_size/3,
      stroke = stroke,
      color = "#c4c4c4",
      alpha = point_opacity
    ) +
    geom_segment(
      data = trajectory_edges,
      aes(
        source_dim_1,
        source_dim_2,
        xend = target_dim_1,
        yend = target_dim_2
      ),
      size = 0.75, linetype = "solid", na.rm = TRUE
    ) +
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