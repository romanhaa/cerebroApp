##----------------------------------------------------------------------------##
## Function to plot expression of multiple genes in separate facets.
##
## NOTES:
## - plotting cells as shape "21" with fill color representing the expression
##   values and setting a border doesn't seem to work with the ggploty()
##   conversion
##----------------------------------------------------------------------------##

pltExpProj2DMultPan <- function(
  df,
  point_size,
  point_opacity,
  plot_order,
  color_scale,
  color_range,
  x_range,
  y_range
) {

  ##
  req(
    "gene" %in% colnames(df)
  )

  ## bring cells in order, either random or highest expression on top
  df <- setRowOrder(df, plot_order)

  ## decide how many panel columns should be used
  ## below 6 panels, use 2 columns, from 6-8 panels use 3 columns
  number_of_genes <- length(unique(df$gene))
  number_of_panel_columns <- ifelse(number_of_genes < 6, 2, 3)

  ## prepare plot
  plot <- ggplot(
      df,
      aes_q(
        x = as.name(colnames(df)[1]),
        y = as.name(colnames(df)[2]),
        color = as.name("level")
      )
    ) +
    geom_point(
      size = point_size/10,
      alpha = point_opacity
    ) +
    lims(x = x_range, y = y_range) +
    theme_bw() +
    facet_wrap(~gene, ncol = number_of_panel_columns)

  ## check if selected color scale
  ## ... selected color scale is "Viridis"
  if ( tolower(color_scale) == 'viridis' ) {

    ## add color scale to plot
    plot <- plot +
      viridis::scale_color_viridis(
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
      scale_color_distiller(
        palette = color_scale,
        limits = color_range,
        oob = scales::squish,
        direction = 1,
        name = "Log-normalised\nexpression",
        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
      )
  }

  ##
  return(plot)
}