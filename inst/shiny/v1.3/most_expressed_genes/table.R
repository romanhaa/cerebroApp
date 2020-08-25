##----------------------------------------------------------------------------##
## Tab: Most expressed genes
##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["most_expressed_genes_table_UI"]] <- renderUI({

  ##
  req(
    input[["most_expressed_genes_selected_group"]]
  )

  ##
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Most expressed genes"),
        cerebroInfoButton("most_expressed_genes_info")
      ),
      uiOutput("most_expressed_genes_table_or_text_UI")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that shows either a table with a switch to toggle sub-filtering
## of results and the corresponding selector, or a text message if data is
## missing.
##----------------------------------------------------------------------------##

output[["most_expressed_genes_table_or_text_UI"]] <- renderUI({

  ##
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
})

##----------------------------------------------------------------------------##
## UI element for sub-filtering of results (if toggled).
##----------------------------------------------------------------------------##

output[["most_expressed_genes_filter_subgroups_UI"]] <- renderUI({
  if ( input[["most_expressed_genes_table_filter_switch"]] == TRUE ) {
    fluidRow()
  } else {
    fluidRow(
      column(12,
        selectInput(
          "most_expressed_genes_table_select_group_level",
          label = "Filter results for subgroup:",
          choices = getGroupLevels(input[["most_expressed_genes_selected_group"]])
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##

output[["most_expressed_genes_table"]] <- DT::renderDataTable(server = FALSE, {

  ##
  req(
    input[["most_expressed_genes_selected_group"]],
    input[["most_expressed_genes_table_select_group_level"]]
  )

  ## filter the table for a specific subgroup only if specified by the user,
  ## otherwise show all results
  results_df <- getMostExpressedGenes(input[["most_expressed_genes_selected_group"]])
  if ( input[["most_expressed_genes_table_filter_switch"]] != TRUE ) {
    results_df <- results_df %>%
      dplyr::filter_at(1, dplyr::all_vars(. == input[["most_expressed_genes_table_select_group_level"]]))
  }

  ## if the table is empty, e.g. because the filtering of results for a specific
  ## subgroup did not work properly, skip the processing and show and empty
  ## table (otherwise the procedure would result in an error)
  if ( nrow(results_df) == 0 ) {

    results_df %>%
    as.data.frame() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()

  ## if there is at least 1 row in the table, create proper table
  } else {

    results_df %>%
    dplyr::rename("% of total expression" = pct) %>%
    prettifyTable(
      filter = list(position = "top", clear = TRUE),
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

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["most_expressed_genes_info"]], {
  showModal(
    modalDialog(
      most_expressed_genes_info[["text"]],
      title = most_expressed_genes_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

most_expressed_genes_info <- list(
  title = "Most expressed genes",
  text = HTML("
    Table of top 100 most expressed genes in each group. For example, if gene XY contributes with 5% to the total expression, that means 5% of all transcripts found in all cells of this sample come from that respective gene. These lists can help to identify/verify the dominant cell types.<br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
