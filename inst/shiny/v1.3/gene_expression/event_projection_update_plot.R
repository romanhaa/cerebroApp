##----------------------------------------------------------------------------##
## Update projection plot when expression_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
observeEvent(expression_projection_data_to_plot(), {
  req(expression_projection_data_to_plot())
  debug_log('--> update gene expression projection', 'v')
  expression_projection_parameters_other[['reset_axes']] <- FALSE
  expression_projection_update_plot(expression_projection_data_to_plot())
})
