##----------------------------------------------------------------------------##
## UI elements to set additional parameters for the projection.
##----------------------------------------------------------------------------##
output[["overview_projection_additional_parameters_UI"]] <- renderUI({
  tagList(
    sliderInput(
      "overview_projection_point_size",
      label = "Point size",
      min = preferences[["scatter_plot_point_size"]][["min"]],
      max = preferences[["scatter_plot_point_size"]][["max"]],
      step = preferences[["scatter_plot_point_size"]][["step"]],
      value = preferences[["scatter_plot_point_size"]][["default"]]
    ),
    sliderInput(
      "overview_projection_point_opacity",
      label = "Point opacity",
      min = preferences[["scatter_plot_point_opacity"]][["min"]],
      max = preferences[["scatter_plot_point_opacity"]][["max"]],
      step = preferences[["scatter_plot_point_opacity"]][["step"]],
      value = preferences[["scatter_plot_point_opacity"]][["default"]]
    ),
    sliderInput(
      "overview_projection_percentage_cells_to_show",
      label = "Show % of cells",
      min = preferences[["scatter_plot_percentage_cells_to_show"]][["min"]],
      max = preferences[["scatter_plot_percentage_cells_to_show"]][["max"]],
      step = preferences[["scatter_plot_percentage_cells_to_show"]][["step"]],
      value = preferences[["scatter_plot_percentage_cells_to_show"]][["default"]]
    ),
    uiOutput('overview_projection_show_group_label_UI')
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_additional_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_projection_additional_parameters_info"]], {
  showModal(
    modalDialog(
      overview_projection_additional_parameters_info[["text"]],
      title = overview_projection_additional_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
# <li><b>Range of X/Y axis (located in dropdown menu above the projection):</b> Set the X/Y axis limits. This is useful when you want to change the aspect ratio of the plot.</li>
overview_projection_additional_parameters_info <- list(
  title = "Additional parameters for projection",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
    </ul>
    "
  )
)
