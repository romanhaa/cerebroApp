##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
overview_projection_data <- reactive({
  req(overview_projection_cells_to_show())
  # message('--> trigger "overview_projection_data"')
  cells_df <- getMetaData()[overview_projection_cells_to_show(),]
  # message(str(cells_df))
  return(cells_df)
})
