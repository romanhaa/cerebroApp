##----------------------------------------------------------------------------##
## Tab: Trajectory
##----------------------------------------------------------------------------##

tab_trajectory <- tabItem(
  tabName = "trajectory",
  shinyjs::inlineCSS("
    #trajectory_details_selected_cells_table .table th {
      text-align: center;
    }
    #states_by_group_table .table th {
      text-align: center;
    }
    "
  ),
  uiOutput("trajectory_select_method_and_name_UI"),
  uiOutput("trajectory_projection_UI"),
  uiOutput("trajectory_selected_cells_table_UI"),
  uiOutput("trajectory_distribution_along_pseudotime_UI"),
  uiOutput("trajectory_states_by_group_UI"),
  uiOutput("trajectory_expression_metrics_UI")
)
