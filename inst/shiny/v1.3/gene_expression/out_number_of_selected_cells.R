##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##
output[["expression_number_of_selected_cells"]] <- renderText({
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if ( is.null(expression_projection_selected_cells()) ) {
    ## manually set counter to 0
    number_of_selected_cells <- 0
  ## ... selection has been made and at least 1 cell is in it
  } else {
    ## get number of selected cells
    number_of_selected_cells <- expression_projection_selected_cells() %>%
      nrow() %>%
      formatC(format = "f", big.mark = ",", digits = 0)
  }
  ## prepare string to show
  paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
})
