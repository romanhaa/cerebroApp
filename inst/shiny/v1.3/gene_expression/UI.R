##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##----------------------------------------------------------------------------##

tab_gene_expression <- tabItem(
  tabName = "geneExpression",
  uiOutput("expression_projection_UI"),
  uiOutput("expression_details_selected_cells_UI"),
  uiOutput("expression_in_selected_cells_UI"),
  uiOutput("expression_by_group_UI"),
  uiOutput("expression_by_gene_UI")
)
