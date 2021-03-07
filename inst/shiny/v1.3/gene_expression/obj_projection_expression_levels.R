##----------------------------------------------------------------------------##
## Expression levels of cells in projection.
##----------------------------------------------------------------------------##
expression_projection_expression_levels <- reactive({
  req(
    expression_projection_cells_to_show(),
    expression_selected_genes()
  )
  # message('--> trigger "expression_projection_expression_levels"')
  if ( length(expression_selected_genes()$genes_to_display_present) == 0 ) {
    expression_levels <- rep(0, length(expression_projection_cells_to_show()))
  } else {
    req(expression_projection_coordinates())
    if (
      ncol(expression_projection_coordinates()) == 2 &&
      input[["expression_projection_genes_in_separate_panels"]] == TRUE &&
      length(expression_selected_genes()$genes_to_display_present) >= 2 &&
      length(expression_selected_genes()$genes_to_display_present) <= 9
    ) {
      expression_matrix <- getExpressionMatrix(
          cells = expression_projection_data()$cell_barcode,
          genes = expression_selected_genes()$genes_to_display_present
        ) %>%
        Matrix::t()
      expression_levels <- list()
      for (i in 1:ncol(expression_matrix)) {
        expression_levels[[colnames(expression_matrix)[i]]] <- as.vector(expression_matrix[,i])
      }
    } else {
      genes <- paste0(expression_selected_genes()$genes_to_display_present, collapse=',')
      message(str(genes))
      raw_result <- httr::GET(url = 'http://localhost:8000', path = glue::glue('test?gene={genes}'))
      raw_content <- rawToChar(raw_result$content)
      expression_levels <- jsonlite::fromJSON(raw_content)
      message(str(expression_levels))
      expression_levels <- expression_levels[expression_projection_cells_to_show()]
      message(str(expression_levels))
    }
  }
  # message(str(expression_levels))
  return(expression_levels)
})
