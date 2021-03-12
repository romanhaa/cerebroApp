##----------------------------------------------------------------------------##
## Select category and content.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of category and specific content,
## which are split because the content depends on which category is selected.
##----------------------------------------------------------------------------##
output[["extra_material_select_category_and_content_UI"]] <- renderUI({
  tagList(
    fluidRow(
      column(
        6,
        uiOutput("extra_material_selected_category_UI")
      ),
      column(
        6,
        uiOutput("extra_material_selected_content_UI")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element to select from which category the content should be shown.
##----------------------------------------------------------------------------##
output[["extra_material_selected_category_UI"]] <- renderUI({
  tagList(
    div(
      HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a category:</strong></h2>')
    ),
    fluidRow(
      column(2),
      column(8,
        selectInput(
          "extra_material_selected_category",
          label = NULL,
          choices = getExtraMaterialCategories(),
          width = "100%"
        )
      ),
      column(2)
    )
  )
})

##----------------------------------------------------------------------------##
## UI element to select which content should be shown.
##----------------------------------------------------------------------------##
output[["extra_material_selected_content_UI"]] <- renderUI({
  req(input[["extra_material_selected_category"]])
  ## if selected category is `tables`
  if (
    input[["extra_material_selected_category"]] == 'tables' &&
    checkForExtraTables() == TRUE
  ) {
    ##
    tagList(
      div(
        HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a table:</strong></h2>')
      ),
      fluidRow(
        column(2),
        column(8,
          selectInput(
            "extra_material_selected_content",
            label = NULL,
            choices = getNamesOfExtraTables(),
            width = "100%"
          )
        ),
        column(2)
      )
    )
  ## if selected category is `plots`
  } else if (
    input[["extra_material_selected_category"]] == 'plots' &&
    checkForExtraPlots() == TRUE
  ) {
    ##
    tagList(
      div(
        HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a plot:</strong></h2>')
      ),
      fluidRow(
        column(2),
        column(8,
          selectInput(
            "extra_material_selected_content",
            label = NULL,
            choices = getNamesOfExtraPlots(),
            width = "100%"
          )
        ),
        column(2)
      )
    )
  }
})
