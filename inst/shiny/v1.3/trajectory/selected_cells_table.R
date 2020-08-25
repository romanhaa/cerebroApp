##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Table of selected cells.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_selected_cells_table_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Table of selected cells"),
        cerebroInfoButton("trajectory_details_selected_cells_table_info")
      ),
      tagList(
        shinyWidgets::materialSwitch(
          inputId = "trajectory_details_selected_cells_table_number_formatting",
          label = "Automatically format numbers:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "trajectory_details_selected_cells_table_color_highlighting",
          label = "Highlight values with colors:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        DT::dataTableOutput("trajectory_details_selected_cells_table")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##

output[["trajectory_details_selected_cells_table"]] <- DT::renderDataTable(server = FALSE, {

  ## don't do anything before these inputs are selected
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  ## collect trajectory data
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  ## check selection
  ## ... selection has not been made or there is not cell in it
  if (
    is.null(plotly::event_data("plotly_selected", source = "trajectory_projection")) ||
    length(plotly::event_data("plotly_selected", source = "trajectory_projection")) == 0
  ) {

    ## prepare empty table
    getMetaData() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()

  ## ... selection has been made and at least 1 cell is in it
  } else {

    ## get info of selected cells and create identifier from X-Y coordinates
    selected_cells <- plotly::event_data("plotly_selected", source = "trajectory_projection") %>%
      dplyr::mutate(identifier = paste0(x, '-', y))

    ## extract cells for table
    cells_df <- cbind(trajectory_data[["meta"]], getMetaData()) %>%
      dplyr::filter(!is.na(pseudotime))
  
    ## filter out non-selected cells with X-Y identifier
    cells_df <- cells_df %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(identifier = paste0(X1, '-', X2)) %>%
      dplyr::filter(identifier %in% selected_cells$identifier) %>%
      dplyr::select(-c(X1, X2, identifier)) %>%
      dplyr::select(cell_barcode, everything())

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
        number_formatting = input[["trajectory_details_selected_cells_table_number_formatting"]],
        color_highlighting = input[["trajectory_details_selected_cells_table_color_highlighting"]],
        hide_long_columns = TRUE,
        download_file_name = "trajectory_details_of_selected_cells"
      )
    }
  }
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_details_selected_cells_table_info"]], {
  showModal(
    modalDialog(
      trajectory_details_selected_cells_table_info$text,
      title = trajectory_details_selected_cells_table_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_details_selected_cells_table_info <- list(
  title = "Details of selected cells",
  text = HTML("
    Table containing meta data (some columns may be hidden, check the 'Column visibility' button) for cells selected in the plot using the box or lasso selection tool. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.
    <h4>Options</h4>
    <b>Show results for all subgroups (no filtering)</b><br>
    When active, the subgroup section element will disappear and instead the table will be shown for all subgroups. Subgroups can still be selected through the dedicated column filter, which also allows to select multiple subgroups at once. While using the column filter is more elegant, it can become laggy with very large tables, hence to option to filter the table beforehand.<br>
    <b>Automatically format numbers</b><br>
    When active, columns in the table that contain different types of numeric values will be formatted based on what they <u>seem</u> to be. The algorithm will look for integers (no decimal values), percentages, p-values, log-fold changes and apply different formatting schemes to each of them. Importantly, this process does that always work perfectly. If it fails and hinders working with the table, automatic formatting can be deactivated.<br>
    <b>Highlight values with colors</b><br>
    Similar to the automatic formatting option, when active, Cerebro will look for known columns in the table (those that contain grouping variables), try to interpret column content, and use colors and other stylistic elements to facilitate quick interpretation of the values. If you prefer the table without colors and/or the identification does not work properly, you can simply deactivate this feature.<br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
