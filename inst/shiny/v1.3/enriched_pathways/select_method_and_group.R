##----------------------------------------------------------------------------##
## Tab: Enriched pathways
##
## Select method and group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of method and group, which are split
## because the group depends on which method is selected.
##----------------------------------------------------------------------------##

output[["enriched_pathways_select_method_and_group_UI"]] <- renderUI({
  tagList(
    fluidRow(
      column(
        6,
        uiOutput("enriched_pathways_selected_method_UI")
      ),
      column(
        6,
        uiOutput("enriched_pathways_selected_group_UI")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element to select from which method the results should be shown.
##----------------------------------------------------------------------------##

output[["enriched_pathways_selected_method_UI"]] <- renderUI({
  tagList(
    div(
      HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a method:</strong></h2>')
    ),
    fluidRow(
      column(2),
      column(8,
        selectInput(
          "enriched_pathways_selected_method",
          label = NULL,
          choices = getMethodsForEnrichedPathways(),
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

output[["enriched_pathways_selected_group_UI"]] <- renderUI({
  req(
    input[["enriched_pathways_selected_method"]]
  )
  tagList(
    div(
      HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a grouping variable:</strong></h2>')
    ),
    fluidRow(
      column(2),
      column(8,
        selectInput(
          "enriched_pathways_selected_group",
          label = NULL,
          choices = getGroupsWithEnrichedPathways(input[["enriched_pathways_selected_method"]]),
          width = "100%"
        )
      ),
      column(2)
    )
  )
})
