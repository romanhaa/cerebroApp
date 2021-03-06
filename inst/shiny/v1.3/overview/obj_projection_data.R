##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
overview_projection_data <- reactive({
  req(overview_projection_cells_to_show())
  if (exists('mode_debugging') && grepl('v', mode_debugging, ignore.case=TRUE)) {
    message('--> trigger "overview_projection_data"')
  }
  cells_df <- getMetaData()[overview_projection_cells_to_show(),]
  if (exists('mode_debugging') && grepl('vv', mode_debugging, ignore.case=TRUE)) {
    message(str(cells_df))
  }
  return(cells_df)
})
