##----------------------------------------------------------------------------##
## Function to plot expression in multiple panels in 2D (for export).
##----------------------------------------------------------------------------##
pltExpProj2DMultPanExp <- function(
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
  plot <- plotExpressionSinglePanel2DExport(
    df = df,
    point_size = point_size,
    point_opacity = point_opacity,
    point_border = point_border,
    color_scale = color_scale,
    color_range = color_range,
    x_range = x_range,
    y_range = y_range
  )
  ## decide how many panel columns should be used
  ## below 6 panels, use 2 columns, from 6-8 panels use 3 columns
  number_of_genes <- length(unique(df$gene))
  number_of_panel_columns <- ifelse(number_of_genes < 6, 2, 3)
  plot <- plot + facet_wrap(~gene, ncol = number_of_panel_columns)
  return(plot)
}
