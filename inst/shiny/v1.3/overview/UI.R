##----------------------------------------------------------------------------##
## Tab: Overview
##----------------------------------------------------------------------------##
tab_overview <- tabItem(
  tabName = "overview",
  ## necessary to ensure alignment of table headers and content
  shinyjs::inlineCSS("
    #overview_details_selected_cells_table .table th {
      text-align: center;
    }
    #overview_details_selected_cells_table .dt-middle {
      vertical-align: middle;
    }
    "
  ),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(script = paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/overview/js_overview_projection_update_plot.js")),
  uiOutput("overview_projection_UI"),
  uiOutput("overview_selected_cells_plot_UI"),
  uiOutput("overview_selected_cells_table_UI")
)
