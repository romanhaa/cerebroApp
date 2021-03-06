##----------------------------------------------------------------------------##
## Upon double-click in brushed are in projection, update X/Y ranges to zoom in.
##----------------------------------------------------------------------------##
observeEvent(input[['overview_projection_dblclick']], {
  brush <- input[['overview_projection_brush']]
  if (!is.null(brush)) {
    overview_projection_ranges$x <- c(brush$xmin, brush$xmax)
    overview_projection_ranges$y <- c(brush$ymin, brush$ymax)
  } else {
    overview_projection_ranges$x <- NULL
    overview_projection_ranges$y <- NULL
  }
})
