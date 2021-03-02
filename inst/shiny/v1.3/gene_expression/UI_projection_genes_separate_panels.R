##----------------------------------------------------------------------------##
## UI elements with switch to plot genes in separate panels.
##----------------------------------------------------------------------------##
output[["expression_projection_genes_in_separate_panels_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "expression_projection_genes_in_separate_panels",
    label = HTML("Show genes in separate panels<br>(experimental)"),
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_genes_in_separate_panels_UI",
  suspendWhenHidden = FALSE
)
