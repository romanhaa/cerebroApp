##----------------------------------------------------------------------------##
## Expression levels of cells in projection.
##----------------------------------------------------------------------------##
expression_projection_expression_levels <- reactive({
  req(
    expression_projection_data(),
    expression_selected_genes()
  )
  # message('--> trigger "expression_projection_expression_levels"')
  if ( length(expression_selected_genes()$genes_to_display_present) == 0 ) {
    expression_levels <- rep(0, nrow(expression_projection_data()))
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
      expression_levels <- unname(getMeanExpressionForCells(
        cells = expression_projection_data()$cell_barcode,
        genes = expression_selected_genes()$genes_to_display_present
      ))
    }
  }
  # message(str(expression_levels))
  return(expression_levels)
})
