##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
overview_projection_data <- reactive({
  req(overview_projection_parameters_cell_filtering())
  parameters <- overview_projection_parameters_cell_filtering()
  cells_df <- cbind(getProjection(parameters[["projection"]]), getMetaData())
  ## remove cells based on group filters
  for ( i in getGroups() ) {
    ## make sure that group exists in meta data (as column) and that selected
    ## groups are not NULL, then subset the data frame
    if ( i %in% colnames(cells_df) ) {
      cells_df <- cells_df[which(cells_df[[i]] %in% parameters[["group_filters"]][[ i ]] ),]
    }
  }
  ## randomly remove cells (if necessary)
  cells_df <- randomlySubsetCells(cells_df, parameters[["pct_cells"]])
  ## put rows in random order
  cells_df <- cells_df[ sample(1:nrow(cells_df)) , ]
  return(cells_df)
})
