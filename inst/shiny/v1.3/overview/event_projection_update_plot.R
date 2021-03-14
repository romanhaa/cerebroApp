##----------------------------------------------------------------------------##
## Update projection plot when overview_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
observeEvent(overview_projection_data_to_plot(), {
  req(overview_projection_data_to_plot())
  debug_log('--> update overview projection', 'v')
  overview_projection_update_plot(overview_projection_data_to_plot())
})
