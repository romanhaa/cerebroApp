##----------------------------------------------------------------------------##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["marker_genes_table_UI"]] <- renderUI({
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_table"]]
  )
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
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_table"]],
    input[["marker_genes_selected_table"]] %in% getGroupsWithMarkerGenes(input[["marker_genes_selected_method"]])
  )
  ## fetch results
  results_type <- getMarkerGenes(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_table"]]
  )
  if ( length(results_type) > 0 ) {
    if ( is.data.frame(results_type) ) {
      fluidRow(
        column(12,
          shinyWidgets::materialSwitch(
            inputId = "marker_genes_table_filter_switch",
            label = "Show results for all subgroups (no pre-filtering):",
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
            value = TRUE,
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
    } else if (
      is.character(results_type) &&
      results_type == "no_markers_found"
    ) {
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
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_table"]],
    input[["marker_genes_selected_table"]] %in% getGroupsWithMarkerGenes(input[["marker_genes_selected_method"]]),
    !is.null(input[["marker_genes_table_filter_switch"]])
  )
  ## fetch results
  results_df <- getMarkerGenes(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_table"]]
  )
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  ## check if pre-filtering is activated and name of first column in table is
  ## one of the registered groups
  ## ... it's not
  if (
    input[["marker_genes_table_filter_switch"]] == TRUE ||
    colnames(results_df)[1] %in% getGroups() == FALSE
  ) {
    ## return nothing (empty row)
    fluidRow()
  ## ... it is
  } else {
    ## check for which groups results exist
    if ( is.character(results_df[[1]]) ) {
      available_groups <- unique(results_df[[1]])
    } else if ( is.factor(results_df[[1]]) ) {
      available_groups <- levels(results_df[[1]])
    }
    ## create input selection for available groups
    fluidRow(
      column(12,
        selectInput(
          "marker_genes_table_select_group_level",
          label = "Filter results for subgroup:",
          choices = available_groups
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##
output[["marker_genes_table"]] <- DT::renderDataTable({
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_table"]],
    input[["marker_genes_selected_table"]] %in% getGroupsWithMarkerGenes(input[["marker_genes_selected_method"]])
  )
  ## fetch results
  results_df <- getMarkerGenes(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_table"]]
  )
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  ## filter the table for a specific subgroup only if specified by the user
  ## (otherwise show all results)
  if (
    input[["marker_genes_table_filter_switch"]] == FALSE &&
    colnames(results_df)[1] %in% getGroups() == TRUE
  ) {
    ## don't proceed if selection of subgroup is not available
    req(input[["marker_genes_table_select_group_level"]])
    ## filter table
    results_df <- results_df[ which(results_df[[1]] == input[["marker_genes_table_select_group_level"]]) , ]
  }
  ## if the table is empty, e.g. because the filtering of results for a specific
  ## subgroup did not work properly, skip the processing and show and empty
  ## table (otherwise the procedure would result in an error)
  if ( nrow(results_df) == 0 ) {
    results_df %>%
    as.data.frame() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()
  ## if there is at least 1 row, create proper table
  } else {
    prettifyTable(
      results_df,
      filter = list(position = "top", clear = TRUE),
      dom = "Bfrtlip",
      show_buttons = TRUE,
      number_formatting = input[["marker_genes_table_number_formatting"]],
      color_highlighting = input[["marker_genes_table_color_highlighting"]],
      hide_long_columns = TRUE,
      download_file_name = paste0(
        "marker_genes_by_",
        input[["marker_genes_selected_method"]], "_",
        input[["marker_genes_selected_table"]]
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
  "No marker genes were identified for any of the subpopulations of this grouping variable."
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["marker_genes_table_no_data"]] <- renderText({
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
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
marker_genes_info <- list(
  title = "Marker genes",
  text = HTML("
    Shown here are the marker genes identified for each group - resembling bulk RNA-seq. These genes should help to functionally interpret the role of a given group of cells or find new markers to purify it.<br>
    Cerebro performs this analysis with the 'FindAllMarkers()' function by Seurat, which compares each group to all other groups combined. Only genes that pass thresholds for log-fold change, the percentage of cells that express the gene, p-value, and adjusted p-values are reported. Statistical analysis can be done using different tests Finally, if data is available, the last column reports for each gene if it is associated with gene ontology term GO:0009986 which is an indicator that the respective gene is present on the cell surface (which could make it more interesting to purify a given population).<br>
    Results from other methods and tools can be manually added to the Cerebro object in which case the description above might not be applicable.
    <h4>Options</h4>
    <b>Show results for all subgroups (no pre-filtering)</b><br>
    When active, the subgroup section element will disappear and instead the table will be shown for all subgroups. Subgroups can still be selected through the dedicated column filter, which also allows to select multiple subgroups at once. While using the column filter is more elegant, it can become laggy with very large tables, hence to option to filter the table beforehand. Please note that this feature only works if the first column was recognized as holding assignments to one of the grouping variables, e.g. 'sample' or 'clusters', otherwise your choice here will be ignored and the whole table shown without pre-filtering.<br>
    <b>Automatically format numbers</b><br>
    When active, columns in the table that contain different types of numeric values will be formatted based on what they <u>seem</u> to be. The algorithm will look for integers (no decimal values), percentages, p-values, log-fold changes and apply different formatting schemes to each of them. Importantly, this process does that always work perfectly. If it fails and hinders working with the table, automatic formatting can be deactivated.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <b>Highlight values with colors</b><br>
    Similar to the automatic formatting option, when active, Cerebro will look for known columns in the table (those that contain grouping variables), try to interpret column content, and use colors and other stylistic elements to facilitate quick interpretation of the values. If you prefer the table without colors and/or the identification does not work properly, you can simply deactivate this feature.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
