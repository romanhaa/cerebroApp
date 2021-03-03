##----------------------------------------------------------------------------##
## UI elements to set X and Y scales in plot. Separate element because it
## requires user input from other UI elements.
##----------------------------------------------------------------------------##
output[["expression_projection_scales_UI"]] <- renderUI({
  req(input[["expression_projection_to_display"]])
  if (
    is.null(input[["expression_projection_to_display"]]) ||
    is.na(input[["expression_projection_to_display"]])
  ) {
    projection_to_display <- availableProjections()[1]
  } else {
    projection_to_display <- input[["expression_projection_to_display"]]
  }
  ## check if projection or trajectory should be shown
  ## ... projection
  if ( input[["expression_projection_to_display"]] %in% availableProjections() ) {
    XYranges <- getXYranges(getProjection(input[["expression_projection_to_display"]]))
  ## ... trajectory
  } else {
    ## split selection into method and name
    selection <- strsplit(input[["expression_projection_to_display"]], split = ' // ')[[1]]
    ## check if method and name exist and don't proceed if not
    req(
      selection[1] %in% getMethodsForTrajectories(),
      selection[2] %in% getNamesOfTrajectories(selection[1])
    )
    ## collect trajectory data
    trajectory_data <- getTrajectory(
      selection[1],
      selection[2]
    )
    XYranges <- getXYranges(trajectory_data[["meta"]])
  }
  tagList(
    sliderInput(
      "expression_projection_scale_x_manual_range",
      label = "Range of X axis",
      min = XYranges$x$min,
      max = XYranges$x$max,
      value = c(XYranges$x$min, XYranges$x$max)
    ),
    sliderInput(
      "expression_projection_scale_y_manual_range",
      label = "Range of Y axis",
      min = XYranges$y$min,
      max = XYranges$y$max,
      value = c(XYranges$y$min, XYranges$y$max)
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_scales_UI",
  suspendWhenHidden = FALSE
)
