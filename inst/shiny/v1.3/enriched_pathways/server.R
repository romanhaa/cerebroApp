##----------------------------------------------------------------------------##
## Tab: Enriched pathways
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## All UI elements.
##----------------------------------------------------------------------------##

output[["enriched_pathways_UI"]] <- renderUI({
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
    ),
    cerebroBox(
      title = tagList(
        boxTitle("Enriched pathways"),
        cerebroInfoButton("enriched_pathways_info")
      ),
      uiOutput("enriched_pathways_table_UI")
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
          choices = sample_data()$getMethodsForEnrichedPathways(),
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
          choices = sample_data()$getGroupsWithEnrichedPathways(input[["enriched_pathways_selected_method"]]),
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

# UI element: display results or alternative text
output[["enriched_pathways_table_UI"]] <- renderUI({
  req(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_group"]]
  )

  ## fetch results
  results <- sample_data()$getEnrichedPathways(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_group"]]
  )

  ## TODO: probably some of these outputs are not needed anymore, check with
  ##       function that generates the output
  if (
    is.character(results) &&
    results == "no_markers_found"
  ) {
    textOutput("enriched_pathways_message_no_markers_found")
  } else if (
    is.character(results) &&
    results == "no_gene_sets_enriched"
  ) {
    textOutput("enriched_pathways_message_no_gene_sets_enriched")
  } else if (
    is.character(results) &&
    ## TODO: adjust output of function to catch this message
    results == "only_one_group_level"
  ) {
    textOutput("enriched_pathways_message_only_one_group_level")
  } else if ( is.data.frame(results) ) {
    fluidRow(
      column(12,
        shinyWidgets::materialSwitch(
          inputId = "enriched_pathways_table_filter_switch",
          label = "Show results for all subgroups (no filtering):",
          value = FALSE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "enriched_pathways_table_number_formatting",
          label = "Automatically format numbers:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "enriched_pathways_table_color_highlighting",
          label = "Highlight numbers with colors:",
          value = FALSE,
          status = "primary",
          inline = TRUE
        )
      ),
      column(12,
        uiOutput("enriched_pathways_filter_subgroups_UI")
      ),
      column(12,
        DT::dataTableOutput("enriched_pathways_table")
      )
    )
  # }
  # } else if (
  #   is.character(results) &&
  #   results == "no_markers_found"
  # ) {
  #   textOutput("enriched_pathways_message_no_markers_found")
  # } else if (
  #   is.character(results) &&
  #   results == "no_gene_sets_enriched"
  # ) {
  #   textOutput("enriched_pathways_message_no_gene_sets_enriched")
  # } else if (
  #   is.character(results) &&
  #   ## TODO: adjust output of function to catch this message
  #   results == "only_one_group_level"
  # ) {
  #   textOutput("enriched_pathways_message_only_one_group_level")
  } else {
    textOutput("enriched_pathways_message_no_data_found")
  }
})

##----------------------------------------------------------------------------##
##
##----------------------------------------------------------------------------##

## UI element: show UI element to filter results for subgroup
output[["enriched_pathways_filter_subgroups_UI"]] <- renderUI({
  req(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_group"]]
  )
  if ( input[["enriched_pathways_table_filter_switch"]] == TRUE ) {
    fluidRow()
  } else {
    fluidRow(
      column(12,
        selectInput(
          "enriched_pathways_table_select_group_level",
          label = "Filter results for subgroup:",
          choices = sample_data()$getGroupLevels(input[["enriched_pathways_selected_group"]])
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
##
##----------------------------------------------------------------------------##

## table
output[["enriched_pathways_table"]] <- DT::renderDataTable(server = FALSE, {
  req(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_group"]],
    input[["enriched_pathways_table_select_group_level"]]
  )

  ## fetch results
  table <- sample_data()$getEnrichedPathways(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_group"]]
  )

  req(
    is.data.frame(table)
  )

  ## filter the table for a specific subgroup only if specified by the user
  ## (otherwise show all results)
  if ( input[["enriched_pathways_table_filter_switch"]] == FALSE ) {
    table <- table %>%
      dplyr::filter_at(1, dplyr::all_vars(. == input[["enriched_pathways_table_select_group_level"]]))
  }

  ## if the table is empty, e.g. because the filtering of results for a specific
  ## subgroup did not work properly, skip the processing and show and empty
  ## table (otherwise the procedure would result in an error)
  if ( nrow(table) == 0 ) {
    table %>%
    as.data.frame() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()
  ## if there is at least 1 row, create proper table
  } else if ( nrow(table) > 0 ) {

    ## check if data frame comes from the enrichR cerebroApp function
    columns_hide <- c()
    if (
      any(grepl(colnames(table), pattern = "Term")) &&
      any(grepl(colnames(table), pattern = "Old.P.value")) &&
      any(grepl(colnames(table), pattern = "Old.Adjusted.P.value"))
    ) {
      columns_hide <- c()
      columns_hide <- c(columns_hide, grep(colnames(table), pattern = "Old.P.value"))
      columns_hide <- c(columns_hide, grep(colnames(table), pattern = "Old.Adjusted.P.value"))
    }

    prettifyTable(
      table,
      filter = list(position = "top", clear = TRUE),
      dom = "Bfrtlip",
      show_buttons = TRUE,
      number_formatting = input[["enriched_pathways_table_number_formatting"]],
      color_highlighting = input[["enriched_pathways_table_color_highlighting"]],
      hide_long_columns = TRUE,
      columns_hide = columns_hide,
      download_file_name = paste0(
        "enriched_pathways_by_",
        input[["enriched_pathways_selected_method"]], "_",
        input[["enriched_pathways_selected_group"]]
      ),
      page_length_default = 20,
      page_length_menu = c(20, 50, 100)
    )
  }

})

##----------------------------------------------------------------------------##
##
##----------------------------------------------------------------------------##

# alternative text messages
output[["enriched_pathways_message_no_markers_found"]] <- renderText({
  "No marker genes were identified for this group, which are required to perform pathway enrichment analysis with Enrichr."
})

output[["enriched_pathways_message_no_gene_sets_enriched"]] <- renderText({
  "No gene sets were found to be enriched (with the selected statistical thresholds) in any group."
})

output[["enriched_pathways_message_only_one_group_level"]] <- renderText({
  "The selected grouping variable consists of a single level which means pathway enrichment analysis cannot be applied."
})

output[["enriched_pathways_message_no_data_found"]] <- renderText({
  "Data not available or not in correct format (data frame)."
})

##----------------------------------------------------------------------------##
##
##----------------------------------------------------------------------------##

# info box
observeEvent(input[["enriched_pathways_info"]], {
  showModal(
    modalDialog(
      enriched_pathways_info[["text"]],
      title = enriched_pathways_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})
