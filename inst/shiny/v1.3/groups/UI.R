##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##
tab_groups <- tabItem(
  tabName = "groups",
  shinyjs::inlineCSS("
    #groups_by_other_group_table .table th {
      text-align: center;
    }
    #groups_by_cell_cycle_table .table th {
      text-align: center;
    }
    "
  ),
  uiOutput("groups_select_group_UI"),
  uiOutput("groups_tree_UI"),
  uiOutput("groups_composition_UI"),
  uiOutput("groups_expression_metrics_UI"),
  uiOutput("groups_cell_cycle_UI")
)
