##----------------------------------------------------------------------------##
## UI elements to choose whether gene(s) or gene sets should be analyzed
##----------------------------------------------------------------------------##
output[["expression_projection_input_type_UI"]] <- renderUI({
  req(
    input[["expression_analysis_mode"]],
    list_of_genes()
  )
  if ( input[["expression_analysis_mode"]] == "Gene(s)" ) {
    selectizeInput(
      'expression_genes_input',
      label = 'Gene(s)',
      choices = data.table::as.data.table(data.frame("Genes" = list_of_genes())),
      multiple = TRUE,
      options = list(
        create = TRUE
      )
    )
  } else if ( input[["expression_analysis_mode"]] == "Gene set" ) {
    selectizeInput(
      'expression_select_gene_set',
      label = 'Gene set',
      choices = data.table::as.data.table(
        data.frame("Gene sets" = c("-", msigdbr:::msigdbr_genesets$gs_name))
      ),
      multiple = FALSE
    )
  }
})
