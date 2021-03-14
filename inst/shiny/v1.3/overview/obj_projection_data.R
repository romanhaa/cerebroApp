##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
overview_projection_data <- reactive({
  req(overview_projection_cells_to_show())
  debug_log('--> trigger "overview_projection_data"', 'v')
  cells_df <- getMetaData()[overview_projection_cells_to_show(),]
  debug_log(str(cells_df), 'vv')
  return(cells_df)
})
