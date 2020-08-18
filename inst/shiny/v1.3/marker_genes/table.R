##----------------------------------------------------------------------------##
## Tab: Marker genes
##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["marker_genes_table_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Marker genes"),
        cerebroInfoButton("marker_genes_info")
      ),
      uiOutput("marker_genes_table_or_text_UI")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that shows table and toggle switches (for sub-filtering of
## results, automatic number formatting, automatic coloring of values), or text
## messages if no marker genes were found or data is missing.
##----------------------------------------------------------------------------##

output[["marker_genes_table_or_text_UI"]] <- renderUI({

  ##
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_group"]]
  )

  ## fetch results
  results <- getMarkerGenes(
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
## UI element for sub-filtering of results if toggled.
##----------------------------------------------------------------------------##

output[["marker_genes_filter_subgroups_UI"]] <- renderUI({

  ##
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_group"]]
  )

  ##
  if ( input[["marker_genes_table_filter_switch"]] == TRUE ) {
    fluidRow()
  } else {
    fluidRow(
      column(12,
        selectInput(
          "marker_genes_table_select_group_level",
          label = "Filter results for subgroup:",
          choices = getGroupLevels(input[["marker_genes_selected_group"]])
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##

output[["marker_genes_table"]] <- DT::renderDataTable(server = FALSE, {

  ##
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_group"]],
    input[["marker_genes_table_select_group_level"]]
  )

  ## fetch results
  table <- getMarkerGenes(
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

##----------------------------------------------------------------------------##
## Alternative text message if no marker genes were found.
##----------------------------------------------------------------------------##

output[["marker_genes_table_no_markers_found"]] <- renderText({
  "No marker genes identified for any of the groups."
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##

output[["marker_genes_no_data"]] <- renderText({
  "Data not available. Possible reasons: Only 1 group in this data set or data not generated."
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

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

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

## TODO: update description
marker_genes_info <- list(
  title = "Marker genes",
  text = p("Shown here are the marker genes identified for each group - resembling bulk RNA-seq. These genes should help to identify the cell type in this sample or find new markers to purify it. In this analysis, each group is compared to all other samples combined. Only genes with a positive average log-fold change of at least 0.25 are reported - meaning only over-expressed genes are shown. Also, marker genes must be expressed in at least 70% of the cells of the respective sample. Statistical analysis is performed using a classical t-test as it has been shown to be very accurate in single cell RNA-seq. Finally, if data is available, the last column reports for each gene if it is associated with gene ontology term GO:0009986 which is an indicator that the respective gene is present on the cell surface (which could make it more interesting to purify a given population).")
)
