##----------------------------------------------------------------------------##
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
    value = ifelse(
      !is.null(getExperiment()$experiment_name),
      getExperiment()$experiment_name,
      'not available'
    ),
    subtitle = "Experiment",
    color = "light-blue"
  )
})

## number of cells
output[["load_data_number_of_cells"]] <- renderValueBox({
  valueBox(
    value = formatC(nrow(data_set()$meta_data), format = "f", big.mark = ",", digits = 0),
    subtitle = "Cells",
    color = "light-blue"
  )
})

## number of grouping variables
output[["load_data_number_of_grouping_variables"]] <- renderValueBox({
  valueBox(
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
    ifelse(
      !is.null(getExperiment()$organism),
      getExperiment()$organism,
      'not available'
    )
  )
})

## date of analysis
## as.character() because the date is otherwise converted to interger
output[["load_data_date_of_analysis"]] <- renderValueBox({
  box(
    title = "Date when data was analyzed",
    width = 5,
    background = "light-blue",
    ifelse(
      !is.null(getExperiment()$date_of_analysis),
      as.character(getExperiment()$date_of_analysis),
      'not available'
    )
  )
})

## date of export
## as.character() because the date is otherwise converted to interger
output[["load_data_date_of_export"]] <- renderValueBox({
  box(
    title = "Date when data was exported",
    width = 5,
    background = "light-blue",
    ifelse(
      !is.null(getExperiment()$date_of_export),
      as.character(getExperiment()$date_of_export),
      'not available'
    )
  )
})
