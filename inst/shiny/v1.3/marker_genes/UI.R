##----------------------------------------------------------------------------##
## Tab: Marker genes
##----------------------------------------------------------------------------##
tab_marker_genes <- tabItem(
  tabName = "markerGenes",
  shinyjs::inlineCSS("
    #marker_genes_table .table th {
      text-align: center;
    }
    "
  ),
  uiOutput("marker_genes_select_method_and_table_UI"),
  uiOutput("marker_genes_table_UI")
)
