##----------------------------------------------------------------------------##
## Tab: Marker genes
##
## Select method and group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of method and group, which are split
## because the group depends on which method is selected.
##----------------------------------------------------------------------------##

output[["marker_genes_select_method_and_group_UI"]] <- renderUI({
  tagList(
    fluidRow(
      column(
        6,
        uiOutput("marker_genes_selected_method_UI")
      ),
      column(
        6,
        uiOutput("marker_genes_selected_group_UI")
      )
    )
  )
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

output[["marker_genes_selected_group_UI"]] <- renderUI({

  ##
  req(
    input[["marker_genes_selected_method"]]
  )

  ##
  tagList(
    div(
      HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a grouping variable:</strong></h2>')
    ),
    fluidRow(
      column(2),
      column(8,
        selectInput(
          "marker_genes_selected_group",
          label = NULL,
          choices = getGroupsWithMarkerGenes(input[["marker_genes_selected_method"]]),
          width = "100%"
        )
      ),
      column(2)
    )
  )
})
