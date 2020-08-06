##----------------------------------------------------------------------------##
## Tab: Marker genes
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## All UI elements.
##----------------------------------------------------------------------------##

output[["marker_genes_UI"]] <- renderUI({
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
    ),
    cerebroBox(
      title = tagList(
        boxTitle("Marker genes"),
        cerebroInfoButton("marker_genes_info")
      ),
      uiOutput("marker_genes_table_UI")
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
          choices = sample_data()$getMethodsForMarkerGenes(),
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
  req(
    input[["marker_genes_selected_method"]]
  )
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
          choices = sample_data()$getGroupsWithMarkerGenes(input[["marker_genes_selected_method"]]),
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
output[["marker_genes_table_UI"]] <- renderUI({
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_group"]]
  )

  ## fetch results
  results <- sample_data()$getMarkerGenes(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_group"]]
  )

  if ( length(results) > 0 ) {
    if ( is.data.frame(results) ) {
      fluidRow(
        column(12,
          shinyWidgets::materialSwitch(
            inputId = "marker_genes_table_filter_switch",
            label = "Show results for all subgroups (no filtering):",
            value = FALSE,
            status = "primary",
            inline = TRUE
          ),
          shinyWidgets::materialSwitch(
            inputId = "marker_genes_table_number_formatting",
            label = "Automatically format numbers:",
            value = TRUE,
            status = "primary",
            inline = TRUE
          ),
          shinyWidgets::materialSwitch(
            inputId = "marker_genes_table_color_highlighting",
            label = "Highlight values with colors:",
            value = FALSE,
            status = "primary",
            inline = TRUE
          )
        ),
        column(12,
          uiOutput("marker_genes_filter_subgroups_UI")
        ),
        column(12,
          DT::dataTableOutput("marker_genes_table")
        )
      )
    } else if ( results == "no_markers_found" ) {
      textOutput("marker_genes_table_no_markers_found")
    }
  } else {
    textOutput("marker_genes_table_no_data")
  }
})

##----------------------------------------------------------------------------##
##
##----------------------------------------------------------------------------##

## UI element: show UI element to filter results for subgroup
output[["marker_genes_filter_subgroups_UI"]] <- renderUI({
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_group"]]
  )
  if ( input[["marker_genes_table_filter_switch"]] == TRUE ) {
    fluidRow()
  } else {
    fluidRow(
      column(12,
        selectInput(
          "marker_genes_table_select_group_level",
          label = "Filter results for subgroup:",
          choices = sample_data()$getGroupLevels(input[["marker_genes_selected_group"]])
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
##
##----------------------------------------------------------------------------##

## table
output[["marker_genes_table"]] <- DT::renderDataTable(server = FALSE, {
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_group"]],
    input[["marker_genes_table_select_group_level"]]
  )

  ## fetch results
  table <- sample_data()$getMarkerGenes(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_group"]]
  )

  req(
    is.data.frame(table)
  )

  ## filter the table for a specific subgroup only if specified by the user
  ## (otherwise show all results)
  if ( input[["marker_genes_table_filter_switch"]] == FALSE ) {
    table <- table %>%
      dplyr::filter_at(1, dplyr::all_vars(. == input[["marker_genes_table_select_group_level"]]))
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
  } else {
    prettifyTable(
      table,
      filter = list(position = "top", clear = TRUE),
      dom = "Bfrtlip",
      show_buttons = TRUE,
      number_formatting = input[["marker_genes_table_number_formatting"]],
      color_highlighting = input[["marker_genes_table_color_highlighting"]],
      hide_long_columns = TRUE,
      download_file_name = paste0(
        "marker_genes_by_",
        input[["marker_genes_selected_method"]], "_",
        input[["marker_genes_selected_group"]]
      ),
      page_length_default = 20,
      page_length_menu = c(20, 50, 100)
    )
  }
})

## alternative text
output[["marker_genes_table_no_markers_found"]] <- renderText({
  "No marker genes identified for any of the groups."
})

## alternative text
output[["marker_genes_no_data"]] <- renderText({
  "Data not available. Possible reasons: Only 1 group in this data set or data not generated."
})

## info box
observeEvent(input[["marker_genes_info"]], {
  showModal(
    modalDialog(
      marker_genes_info[["text"]],
      title = marker_genes_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})
