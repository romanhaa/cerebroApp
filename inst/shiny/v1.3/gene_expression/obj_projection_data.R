##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
expression_projection_data <- reactive({
  req(expression_projection_cells_to_show())
  # message('--> trigger "expression_projection_data"')
  cells_df <- getMetaData()[expression_projection_cells_to_show(),]
  # message(str(cells_df))
  return(cells_df)
})
