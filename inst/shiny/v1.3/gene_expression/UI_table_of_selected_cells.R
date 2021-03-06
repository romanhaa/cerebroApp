##----------------------------------------------------------------------------##
## Table for details of selected cells.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with toggle switches (for automatic number formatting and
## coloring) and table.
##----------------------------------------------------------------------------##
output[["expression_details_selected_cells_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Details of selected cells"),
        cerebroInfoButton("expression_details_selected_cells_info")
      ),
      tagList(
        shinyWidgets::materialSwitch(
          inputId = "expression_details_selected_cells_number_formatting",
          label = "Automatically format numbers:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "expression_details_selected_cells_color_highlighting",
          label = "Highlight values with colors:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        DT::dataTableOutput("expression_details_selected_cells")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##
output[["expression_details_selected_cells"]] <- DT::renderDataTable({
  req(
    expression_projection_data(),
    expression_projection_coordinates(),
    expression_projection_expression_levels()
  )
  selected_cells <- expression_projection_selected_cells()
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if ( is.null(selected_cells) ) {
    ## prepare empty table
    getMetaData() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()
  ## ... selection has been made and at least 1 cell is in it
  } else {
    cells_df <- bind_cols(
      expression_projection_coordinates(),
      expression_projection_data()
    )
    if (is.list(expression_projection_expression_levels())) {
      cells_df$level <- do.call(cbind, expression_projection_expression_levels()) %>%
        Matrix::rowMeans()
    } else {
      cells_df$level <- expression_projection_expression_levels()
    }
    ## filter out non-selected cells with X-Y identifier and select some meta
    ## data
    cells_df <- cells_df %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(identifier = paste0(X1, '-', X2)) %>%
      dplyr::filter(identifier %in% selected_cells$identifier) %>%
      dplyr::select(-c(X1, X2, identifier)) %>%
      dplyr::rename(expression_level = level) %>%
      dplyr::select(cell_barcode, expression_level, everything())
    ## check how many cells are left after filtering
    ## ... no cells are left
    if ( nrow(cells_df) == 0 ) {
      ## prepare empty table
      getMetaData() %>%
      dplyr::slice(0) %>%
      prepareEmptyTable()
    ## ... at least 1 cell is left
    } else {
      ## prepare proper table
      prettifyTable(
        cells_df,
        filter = list(position = "top", clear = TRUE),
        dom = "Brtlip",
        show_buttons = TRUE,
        number_formatting = input[["expression_details_selected_cells_number_formatting"]],
        color_highlighting = input[["expression_details_selected_cells_color_highlighting"]],
        hide_long_columns = TRUE,
        download_file_name = "expression_details_of_selected_cells"
      )
    }
  }
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_details_selected_cells_info"]], {
  showModal(
    modalDialog(
      expression_details_selected_cells_info$text,
      title = expression_details_selected_cells_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_details_selected_cells_info <- list(
  title = "Details of selected cells",
  text = HTML("
    Table containing (average) expression values of selected genes as well as selected meta data (sample, cluster, number of transcripts, number of expressed genes) for cells selected in the plot using the box or lasso selection tool. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.
    <h4>Options</h4>
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
