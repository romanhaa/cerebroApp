##----------------------------------------------------------------------------##
## Hover info of cells in projection.
##----------------------------------------------------------------------------##
overview_projection_hover_info <- reactive({
  req(
    hover_info_projections(),
    overview_projection_cells_to_show()
  )
  debug_log('--> trigger "overview_projection_hover_info"', 'v')
  if (
    !is.null(preferences[["show_hover_info_in_projections"]]) &&
    preferences[['show_hover_info_in_projections']] == TRUE
  ) {
    hover_info <- hover_info_projections()[overview_projection_cells_to_show()]
  } else {
    hover_info <- hover_info_projections()
  }
  debug_log(str(hover_info), 'vv')
  return(hover_info)
})
