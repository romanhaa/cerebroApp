##----------------------------------------------------------------------------##
## Tab: Enriched pathways
##----------------------------------------------------------------------------##

tab_enriched_pathways <- tabItem(
  tabName = "enrichedPathways",
  shinyjs::inlineCSS("
    #enriched_pathways_table .table th {
      text-align: center;
    }
    #enriched_pathways_table .dt-middle {
      vertical-align: middle;
    }
    "
  ),
  uiOutput("enriched_pathways_UI")
)
