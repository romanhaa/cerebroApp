##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
overview_projection_cells_to_show <- reactive({
  req(input[["overview_projection_percentage_cells_to_show"]])
  # message('--> trigger "overview_projection_cells_to_show"')
  groups <- getGroups()
  ## require group filters UI elements and at least 1 group level to be selected
  for ( i in groups ) {
    req(input[[paste0("overview_projection_group_filter_", i)]])
  }
  pct_cells <- input[["overview_projection_percentage_cells_to_show"]]
  group_filters <- list()
  ## store group filters
  for ( i in groups ) {
    group_filters[[i]] <- input[[paste0("overview_projection_group_filter_", i)]]
  }
  cells_df <- getMetaData() %>%
    dplyr::mutate(row_id = row_number())
  ## remove cells based on group filters
  for ( i in groups ) {
    ## make sure that group exists in meta data (as column) and that selected
    ## groups are not NULL, then subset the data frame
    if ( i %in% colnames(cells_df) ) {
      cells_df <- cells_df[which(cells_df[[i]] %in% group_filters[[i]] ),]
    }
  }
  cells_df <- cells_df %>%
    dplyr::select(cell_barcode, row_id)
  ## randomly remove cells (if necessary)
  cells_df <- randomlySubsetCells(cells_df, pct_cells)
  ## put rows in random order
  cells_df <- cells_df[ sample(1:nrow(cells_df)) , ]
  cells_to_show <- cells_df$row_id
  # message(str(cells_to_show))
  return(cells_to_show)
})
