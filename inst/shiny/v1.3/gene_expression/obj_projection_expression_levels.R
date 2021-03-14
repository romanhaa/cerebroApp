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
      expression_matrix <- data_set()$expression[expression_selected_genes()$genes_to_display_present, , drop=FALSE]
      expression_matrix <- Matrix::t(expression_matrix)
      expression_levels <- list()
      for (i in 1:ncol(expression_matrix)) {
        expression_levels[[colnames(expression_matrix)[i]]] <- as.vector(expression_matrix[,i])
      }
    } else if (length(expression_selected_genes()$genes_to_display_present) == 1) {
      expression_levels <- data_set()$expression[expression_selected_genes()$genes_to_display_present,]
      expression_levels <- unname(expression_levels)
      expression_levels <- expression_levels[isolate(expression_projection_cells_to_show())]
    } else if (length(expression_selected_genes()$genes_to_display_present) >= 2) {
      expression_levels <- data_set()$expression[expression_selected_genes()$genes_to_display_present,]
      expression_levels <- as.matrix(expression_levels)
      expression_levels <- colMeans(expression_levels)
      expression_levels <- unname(expression_levels)
      expression_levels <- expression_levels[isolate(expression_projection_cells_to_show())]
    }
  }
  # message(str(expression_levels))
  return(expression_levels)
})
