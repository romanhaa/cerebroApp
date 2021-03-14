##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##
output[["overview_details_selected_cells_table"]] <- DT::renderDataTable({
  ## don't proceed without these inputs
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_to_display"]] %in% availableProjections(),
    overview_projection_selected_cells()
  )
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if ( nrow(overview_projection_selected_cells())==0 ) {
    ## prepare empty table
    getMetaData() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()
  ## ... selection has been made and at least 1 cell is in it
  } else {
    ## prepare proper table
    prettifyTable(
      overview_projection_selected_cells(),
      filter = list(position = "top", clear = TRUE),
      dom = "Brtlip",
      show_buttons = TRUE,
      number_formatting = input[["overview_details_selected_cells_table_number_formatting"]],
      color_highlighting = input[["overview_details_selected_cells_table_color_highlighting"]],
      hide_long_columns = TRUE,
      download_file_name = "overview_details_of_selected_cells"
    )
  }
})
