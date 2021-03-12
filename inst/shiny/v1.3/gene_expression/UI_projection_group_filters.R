##----------------------------------------------------------------------------##
## UI elements to set group filters.
##----------------------------------------------------------------------------##
output[["expression_projection_group_filters_UI"]] <- renderUI({
  group_filters <- list()

  for ( i in getGroups() ) {
    group_filters[[i]] <- shinyWidgets::pickerInput(
      paste0("expression_projection_group_filter_", i),
      label = i,
      choices = getGroupLevels(i),
      selected = getGroupLevels(i),
      options = list(
        "actions-box" = TRUE
      ),
      multiple = TRUE
    )
  }
  group_filters
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_group_filters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_projection_group_filters_info"]], {
  showModal(
    modalDialog(
      expression_projection_group_filters_info$text,
      title = expression_projection_group_filters_info$title,
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
expression_projection_group_filters_info <- list(
  title = "Group filters for gene (set) expression",
  text = HTML("
    The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that are pass all filters (for each grouping variable) are shown in the projection, the expression by group, and expression by pseudotime (if applicable).
    "
  )
)
