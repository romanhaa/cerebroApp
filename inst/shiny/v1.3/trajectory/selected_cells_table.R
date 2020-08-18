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
          value = FALSE,
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
    to_plot <- cbind(
        trajectory_data[["meta"]],
        getMetaData()
      ) %>%
      dplyr::filter(!is.na(pseudotime))
  
    ## filter out non-selected cells with X-Y identifier
    table <- to_plot %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(identifier = paste0(X1, '-', X2)) %>%
      dplyr::filter(identifier %in% selected_cells$identifier) %>%
      dplyr::select(-c(X1, X2, identifier)) %>%
      dplyr::select(cell_barcode, everything())

    ## check how many cells are left after filtering
    ## ... no cells are left
    if ( nrow(table) == 0 ) {

      ## prepare empty table
      getMetaData() %>%
      dplyr::slice(0) %>%
      prepareEmptyTable()

    ## ... at least 1 cell is left
    } else {

      ## prepare proper table
      prettifyTable(
        table,
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
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

## TODO: update text
trajectory_details_selected_cells_table_info <- list(
  title = "Details of selected cells",
  text = p("Table containing meta data (some columns may be hidden, check the 'Column visibility' button) for cells selected in the plot using the box or lasso selection tool. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.")
)
