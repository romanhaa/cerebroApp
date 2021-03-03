##----------------------------------------------------------------------------##
## Select method and table (group).
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of method and group, which are split
## because the group depends on which method is selected.
##----------------------------------------------------------------------------##
output[["marker_genes_select_method_and_table_UI"]] <- renderUI({
   if (
    !is.null(getMethodsForMarkerGenes()) &&
    length(getMethodsForMarkerGenes()) > 0
  ) {
    tagList(
      fluidRow(
        column(
          6,
          uiOutput("marker_genes_selected_method_UI")
        ),
        column(
          6,
          uiOutput("marker_genes_selected_table_UI")
        )
      )
    )
  } else {
    fluidRow(
      cerebroBox(
        title = boxTitle("Marker genes"),
        textOutput("marker_genes_message_no_method_found")
      )
    )
  }
})

##----------------------------------------------------------------------------##
## UI element to select from which method the results should be shown.
##----------------------------------------------------------------------------##
output[["marker_genes_selected_method_UI"]] <- renderUI({
  tagList(
    div(
      HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a method:</strong></h2>')
    ),
    fluidRow(
      column(2),
      column(8,
        selectInput(
          "marker_genes_selected_method",
          label = NULL,
          choices = getMethodsForMarkerGenes(),
          width = "100%"
        )
      ),
      column(2)
    )
  )
})

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##
output[["marker_genes_selected_table_UI"]] <- renderUI({
  req(input[["marker_genes_selected_method"]])
  tagList(
    div(
      HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a table:</strong></h2>')
    ),
    fluidRow(
      column(2),
      column(8,
        selectInput(
          "marker_genes_selected_table",
          label = NULL,
          choices = getGroupsWithMarkerGenes(input[["marker_genes_selected_method"]]),
          width = "100%"
        )
      ),
      column(2)
    )
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["marker_genes_message_no_method_found"]] <- renderText({
  "No data available."
})
