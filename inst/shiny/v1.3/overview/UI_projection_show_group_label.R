##----------------------------------------------------------------------------##
## UI elements with switch to show group labels in projection.
##----------------------------------------------------------------------------##
output[["overview_projection_show_group_label_UI"]] <- renderUI({
  req(input[["overview_projection_point_color"]])
  if ( input[["overview_projection_point_color"]] %in% getGroups() ) {
    shinyWidgets::awesomeCheckbox(
      inputId = "overview_projection_show_group_label",
      label = "Plot group labels in exported PDF",
      value = TRUE
    )
  }
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_show_group_label_UI",
  suspendWhenHidden = FALSE
)
