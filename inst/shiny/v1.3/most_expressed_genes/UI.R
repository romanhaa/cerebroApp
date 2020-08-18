##----------------------------------------------------------------------------##
## Tab: Most expressed genes
##----------------------------------------------------------------------------##

tab_most_expressed_genes <- tabItem(
  tabName = "mostExpressedGenes",
  shinyjs::inlineCSS("
    #most_expressed_genes_table .table th {
      text-align: center;
    }
    "
  ),
  uiOutput("most_expressed_genes_select_group_UI"),
  uiOutput("most_expressed_genes_table_UI")
)
