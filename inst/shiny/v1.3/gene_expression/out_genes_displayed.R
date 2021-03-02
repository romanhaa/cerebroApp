##----------------------------------------------------------------------------##
## Text showing which genes are present and missing.
##----------------------------------------------------------------------------##
output[["expression_genes_displayed"]] <- renderText({
  ## don't proceed without these inputs
  req(expression_selected_genes())
  ## prepare text output from reactive data
  paste0(
    "<b>Showing expression for ",
    length(expression_selected_genes()[["genes_to_display_present"]]), " gene(s):</b><br>",
    paste0(expression_selected_genes()[["genes_to_display_present"]], collapse = ", "),
    "<br><br><b>",
    length(expression_selected_genes()[["genes_to_display_missing"]]),
    " gene(s) are not in data set: </b><br>",
    paste0(expression_selected_genes()[["genes_to_display_missing"]], collapse = ", ")
  )
})
