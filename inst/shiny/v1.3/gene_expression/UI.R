##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##----------------------------------------------------------------------------##
js_code_gene_expression_projection <- readr::read_file(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_expression/js_projection_update_plot.js")
)

tab_gene_expression <- tabItem(
  tabName = "geneExpression",
  ## necessary to ensure alignment of table headers and content
  shinyjs::inlineCSS("
    #expression_details_selected_cells .table th {
      text-align: center;
    }
    #expression_details_selected_cells .dt-middle {
      vertical-align: middle;
    }
    "
  ),
  shinyjs::extendShinyjs(
    text = js_code_gene_expression_projection,
    functions = c(
      "expressionProjectionUpdatePlot2D",
      "expressionProjectionUpdatePlot2DMultiPanel",
      "expressionProjectionUpdatePlot3D"
    )
  ),
  uiOutput("expression_projection_UI"),
  uiOutput("expression_details_selected_cells_UI"),
  uiOutput("expression_in_selected_cells_UI"),
  uiOutput("expression_by_group_UI"),
  uiOutput("expression_by_gene_UI")#,
  # uiOutput("expression_by_pseudotime_UI")
)
