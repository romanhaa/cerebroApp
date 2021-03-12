##----------------------------------------------------------------------------##
## UI elements to set additional plotting parameters.
##----------------------------------------------------------------------------##
output[["expression_projection_additional_parameters_UI"]] <- renderUI({
  tagList(
    selectInput(
      "expression_projection_plotting_order",
      label = "Plotting order",
      choices = c("Random", "Highest expression on top"),
      selected = "Random"
    ),
    sliderInput(
      "expression_projection_point_size",
      label = "Point size",
      min = preferences[["scatter_plot_point_size"]][["min"]],
      max = preferences[["scatter_plot_point_size"]][["max"]],
      step = preferences[["scatter_plot_point_size"]][["step"]],
      value = preferences[["scatter_plot_point_size"]][["default"]]
    ),
    sliderInput(
      "expression_projection_point_opacity",
      label = "Point opacity",
      min = preferences[["scatter_plot_point_opacity"]][["min"]],
      max = preferences[["scatter_plot_point_opacity"]][["max"]],
      step = preferences[["scatter_plot_point_opacity"]][["step"]],
      value = preferences[["scatter_plot_point_opacity"]][["default"]]
    ),
    sliderInput(
      "expression_projection_percentage_cells_to_show",
      label = "Show % of cells",
      min = preferences[["scatter_plot_percentage_cells_to_show"]][["min"]],
      max = preferences[["scatter_plot_percentage_cells_to_show"]][["max"]],
      step = preferences[["scatter_plot_percentage_cells_to_show"]][["step"]],
      value = preferences[["scatter_plot_percentage_cells_to_show"]][["default"]]
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_additional_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_projection_additional_parameters_info"]], {
  showModal(
    modalDialog(
      expression_projection_additional_parameters_info$text,
      title = expression_projection_additional_parameters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_projection_additional_parameters_info <- list(
  title = "Additional parameters for gene (set) expression",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Plotting order:</b> Cells can be plotted in random order or so that cells with highest expression are on top.</li>
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
    </ul>
    "
  )
)
