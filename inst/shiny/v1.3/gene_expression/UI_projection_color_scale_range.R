##----------------------------------------------------------------------------##
## UI elements to set color scale range.
##----------------------------------------------------------------------------##
output[["expression_projection_color_range_UI"]] <- renderUI({
  req(expression_projection_expression_levels())
  ## get range of expression levels
  if (input[["expression_projection_genes_in_separate_panels"]] == TRUE) {
    expression_levels <- c()
    for (i in length(expression_projection_expression_levels())) {
      expression_levels <- c(
        expression_levels,
        expression_projection_expression_levels()
      )
    }
    expression_range <- range(expression_levels)
  } else {
    expression_range <- range(expression_projection_expression_levels())
  }
  ## adjust expression range for color scale
  ## ... there is no range (from 0 to 0)
  if (
    expression_range[1] == 0 &&
    expression_range[2] == 0
  ) {
    ## set range to 0-1
    expression_range[2] <- 1
  ## ... otherwise
  } else {
    ## round min and max values to 2 digits
    expression_range <- round(expression_range, digits = 2)
  }
  sliderInput(
    "expression_projection_color_range",
    label = "Range of color scale",
    min = expression_range[1],
    max = expression_range[2],
    value = expression_range
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_color_range_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_projection_color_scale_info"]], {
  showModal(
    modalDialog(
      expression_projection_color_scale_info$text,
      title = expression_projection_color_scale_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_projection_color_scale_info <- list(
  title = "Color scale for gene (set) expression",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Color scale:</b> Choose your prefered color scale.</li>
      <li><b>Range of color scale:</b> Using the sliders, you can set the limits for the color scale. Values outside the scale will be shown in the color corresponding to the min/max value, respectively.</li>
    </ul>
    "
  )
)
