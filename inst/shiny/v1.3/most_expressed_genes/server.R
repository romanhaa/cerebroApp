##----------------------------------------------------------------------------##
## Tab: Most expressed genes
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## All UI elements.
##----------------------------------------------------------------------------##

output[["most_expressed_genes_UI"]] <- renderUI({
  tagList(
    uiOutput("most_expressed_genes_selected_group_UI"),
    cerebroBox(
      title = tagList(
        boxTitle("Most expressed genes"),
        cerebroInfoButton("most_expressed_genes_info")
      ),
      uiOutput("most_expressed_genes_table_UI")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##

output[["most_expressed_genes_selected_group_UI"]] <- renderUI({
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
          choices = sample_data()$getGroupsWithMostExpressedGenes(),
          width = "100%"
        )
      ),
      column(2)
    )
  )
})

##----------------------------------------------------------------------------##
##
##----------------------------------------------------------------------------##

## UI element
output[["most_expressed_genes_table_UI"]] <- renderUI({
  req(
    input[["most_expressed_genes_selected_group"]]
  )
  if ( length(sample_data()$getGroupsWithMostExpressedGenes()) > 0 ) {
    fluidRow(
      column(12,
        shinyWidgets::materialSwitch(
          inputId = "most_expressed_genes_table_filter_switch",
          label = "Show results for all subgroups (no filtering):",
          value = FALSE,
          status = "primary",
          inline = TRUE
        )
      ),
      column(12,
        uiOutput("most_expressed_genes_filter_subgroups_UI")
      ),
      column(12,
        DT::dataTableOutput("most_expressed_genes_table")
      )
    )
  } else {
    textOutput("most_expressed_genes_no_data")
  }
})

##----------------------------------------------------------------------------##
##
##----------------------------------------------------------------------------##

## UI element: show UI element to filter results for subgroup
output[["most_expressed_genes_filter_subgroups_UI"]] <- renderUI({
  if ( input[["most_expressed_genes_table_filter_switch"]] == TRUE ) {
    fluidRow()
  } else {
    fluidRow(
      column(12,
        selectInput(
          "most_expressed_genes_table_select_group_level",
          label = "Filter results for subgroup:",
          choices = sample_data()$getGroupLevels(input[["most_expressed_genes_selected_group"]])
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
## Table with data
##----------------------------------------------------------------------------##

## table
output[["most_expressed_genes_table"]] <- DT::renderDataTable(server = FALSE, {
  req(
    input[["most_expressed_genes_selected_group"]],
    input[["most_expressed_genes_table_select_group_level"]]
  )

  ## filter the table for a specific subgroup only if specified by the user,
  ## otherwise show all results
  if ( input[["most_expressed_genes_table_filter_switch"]] == TRUE ) {
    table <- sample_data()$getMostExpressedGenes(input[["most_expressed_genes_selected_group"]])
  } else {
    table <- sample_data()$getMostExpressedGenes(input[["most_expressed_genes_selected_group"]]) %>%
      dplyr::filter_at(1, dplyr::all_vars(. == input[["most_expressed_genes_table_select_group_level"]]))
  }

  ## if the table is empty, e.g. because the filtering of results for a specific
  ## subgroup did not work properly, skip the processing and show and empty
  ## table (otherwise the procedure would result in an error)
  if ( nrow(table) == 0 ) {
    table %>%
    as.data.frame() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()
  ## if there is at least 1 row in the table, create proper table
  } else {
    table %>%
    dplyr::mutate(pct = formattable::percent(pct/100, digits = 2)) %>%
    dplyr::rename(
      # "group" = 1,
      "% of total expression" = pct
    ) %>%
    prettifyTable(
      filter = "none",
      dom = "Bfrtlip",
      show_buttons = TRUE,
      number_formatting = TRUE,
      color_highlighting = TRUE,
      hide_long_columns = FALSE,
      columns_percentage = 3,
      download_file_name = paste0(
        "most_expressed_genes_by_",
        input[["most_expressed_genes_selected_group"]]
      ),
      page_length_default = 20,
      page_length_menu = c(20, 50, 100)
    )
  }
})

## alternative text
output[["most_expressed_genes_no_data"]] <- renderText({
  "Data not available."
})

## info box
observeEvent(input[["most_expressed_genes_info"]], {
  showModal(
    modalDialog(
      most_expressed_genes_info[["text"]],
      title = most_expressed_genes_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})
