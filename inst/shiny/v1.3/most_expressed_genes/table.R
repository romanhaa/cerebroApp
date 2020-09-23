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
        label = "Show results for all subgroups (no pre-filtering):",
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

  ##
  req(
    input[["most_expressed_genes_selected_group"]],
    !is.null(input[["most_expressed_genes_table_filter_switch"]])
  )

  ## fetch results
  results_df <- getMostExpressedGenes(
    input[["most_expressed_genes_selected_group"]]
  )

  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))

  ## check if pre-filtering is activated and name of first column in table is
  ## one of the registered groups
  ## ... it's not
  if (
    input[["most_expressed_genes_table_filter_switch"]] == TRUE ||
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

    fluidRow(
      column(12,
        selectInput(
          "most_expressed_genes_table_select_group_level",
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

output[["most_expressed_genes_table"]] <- DT::renderDataTable(server = FALSE, {

  ##
  req(
    input[["most_expressed_genes_selected_group"]]
  )

  ## fetch results
  results_df <- getMostExpressedGenes(
    input[["most_expressed_genes_selected_group"]]
  )

  ## don't proceed if input is not a data frame
  req(
    is.data.frame(results_df)
  )

  ## filter the table for a specific subgroup only if specified by the user,
  ## otherwise show all results
  if (
    input[["most_expressed_genes_table_filter_switch"]] == FALSE &&
    colnames(results_df)[1] %in% getGroups() == TRUE
  ) {

    ## don't proceed if selection of subgroup is not available
    req(input[["most_expressed_genes_table_select_group_level"]])

    ## filter table
    results_df <- results_df[ which(results_df[[1]] == input[["most_expressed_genes_table_select_group_level"]]) , ]
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
    Table of top 100 most expressed genes in each group. For example, if gene XY contributes with 5% to the total expression, that means 5% of all transcripts found in all cells of this sample come from that respective gene. These lists can help to identify/verify the dominant cell types.
    <h4>Options</h4>
    <b>Show results for all subgroups (no pre-filtering)</b><br>
    When active, the subgroup section element will disappear and instead the table will be shown for all subgroups. Subgroups can still be selected through the dedicated column filter, which also allows to select multiple subgroups at once. While using the column filter is more elegant, it can become laggy with very large tables, hence to option to filter the table beforehand. Please note that this feature only works if the first column was recognized as holding assignments to one of the grouping variables, e.g. 'sample' or 'clusters', otherwise your choice here will be ignored and the whole table shown without pre-filtering.<br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
