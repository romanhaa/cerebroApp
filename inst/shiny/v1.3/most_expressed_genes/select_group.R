##----------------------------------------------------------------------------##
## Tab: Most expressed genes
##
## Select group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##

output[["most_expressed_genes_select_group_UI"]] <- renderUI({

  ## ...
  if (
    !is.null(getGroupsWithMostExpressedGenes()) &&
    length(getGroupsWithMostExpressedGenes()) > 0
  ) {
    tagList(
      div(
        HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a grouping variable:</strong></h2>')
      ),
      fluidRow(
        column(2),
        column(8,
          selectInput(
            "most_expressed_genes_selected_group",
            label = NULL,
            choices = getGroupsWithMostExpressedGenes(),
            width = "100%"
          )
        ),
        column(2)
      )
    )
  } else {
    fluidRow(
      cerebroBox(
        title = boxTitle("Most expressed genes"),
        textOutput("most_expressed_genes_message_no_data_found")
      )
    )
  }
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##

output[["most_expressed_genes_message_no_data_found"]] <- renderText({
  "No data available."
})
