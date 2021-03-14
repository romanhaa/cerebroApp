##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
expression_projection_data <- reactive({
  req(expression_projection_cells_to_show())
  debug_log('--> trigger "expression_projection_data"', 'v')
  cells_df <- getMetaData()[expression_projection_cells_to_show(),]
  debug_log(str(cells_df), 'vv')
  return(cells_df)
})
