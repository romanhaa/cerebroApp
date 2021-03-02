##----------------------------------------------------------------------------##
## Hover info for cells in projection.
##----------------------------------------------------------------------------##
expression_projection_hover_info <- reactive({
  req(
    hover_info_projections(),
    expression_projection_cells_to_show()
  )
  # message('--> trigger "expression_projection_hover_info"')
  if (
    !is.null(preferences[["show_hover_info_in_projections"]]) &&
    preferences[['show_hover_info_in_projections']] == TRUE
  ) {
    hover_info <- hover_info_projections()[expression_projection_cells_to_show()]
  } else {
    hover_info <- hover_info_projections()
  }
  # message(str(hover_info))
  return(hover_info)
})
