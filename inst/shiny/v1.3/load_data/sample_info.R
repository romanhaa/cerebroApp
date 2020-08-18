##----------------------------------------------------------------------------##
## Tab: Load data
##
## Sample info.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI elements that show some basic information about the loaded data set.
##----------------------------------------------------------------------------##

output[["load_data_sample_info_UI"]] <- renderUI({
  tagList(
    fluidRow(
      valueBoxOutput("load_data_experiment_name"),
      valueBoxOutput("load_data_number_of_cells"),
      valueBoxOutput("load_data_number_of_grouping_variables")
    ),
    fluidRow(
      valueBoxOutput("load_data_organism"),
      valueBoxOutput("load_data_date_of_analysis"),
      valueBoxOutput("load_data_date_of_export")
    )
  )
})

##----------------------------------------------------------------------------##
## Value boxes that show:
## - experiment name
## - number of cells in data set
## - number of grouping variables
## - organism
## - date of analysis
## - date of export
##----------------------------------------------------------------------------##

## experiment name
output[["load_data_experiment_name"]] <- renderValueBox({
  valueBox(
    value = getExperiment()$experiment_name,
    subtitle = "Experiment",
    color = "light-blue"
  )
})

## number of cells
output[["load_data_number_of_cells"]] <- renderValueBox({
  valueBox(
    # value = if ( !is.null(sample_data()) ) formatC(ncol(getExpression()), format = "f", big.mark = ",", digits = 0) else 0,
    value = formatC(ncol(getExpression()), format = "f", big.mark = ",", digits = 0),
    subtitle = "Cells",
    color = "light-blue"
  )
})

## number of grouping variables
output[["load_data_number_of_grouping_variables"]] <- renderValueBox({
  valueBox(
    # value = ifelse(!is.null(sample_data()), length(getGroups()), 0),
    value = length(getGroups()),
    subtitle = "Grouping variables",
    color = "light-blue"
  )
})

## organism
output[["load_data_organism"]] <- renderValueBox({
  box(
    title = "Organism",
    width = 5,
    background = "light-blue",
    getExperiment()$organism
  )
})

## date of analysis
output[["load_data_date_of_analysis"]] <- renderValueBox({
  box(
    title = "Date when data was analyzed",
    width = 5,
    background = "light-blue",
    getExperiment()$date_of_analysis
  )
})

## date of export
output[["load_data_date_of_export"]] <- renderValueBox({
  box(
    title = "Date when data was exported",
    width = 5,
    background = "light-blue",
    getExperiment()$date_of_export
  )
})
