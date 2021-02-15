##----------------------------------------------------------------------------##
## Hover info.
##----------------------------------------------------------------------------##
overview_projection_hover_info <- reactive({
  req(overview_projection_data())
  cells_df <- overview_projection_data()
  hover_info <- buildHoverInfoForProjections(cells_df)
  hover_info <- setNames(hover_info, cells_df$cell_barcode)
  return(hover_info)
})
