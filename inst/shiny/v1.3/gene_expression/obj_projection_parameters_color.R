##----------------------------------------------------------------------------##
## Collect color parameters for projection plot.
##----------------------------------------------------------------------------##
expression_projection_parameters_color <- reactive({
  ## require input UI elements
  req(
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_range"]]
  )
  debug_log('--> trigger "expression_projection_parameters_color"', 'v')
  ## collect parameters
  parameters <- list(
    color_scale = input[["expression_projection_color_scale"]],
    color_range = input[["expression_projection_color_range"]]
  )
  debug_log(str(parameters), 'vv')
  return(parameters)
})
