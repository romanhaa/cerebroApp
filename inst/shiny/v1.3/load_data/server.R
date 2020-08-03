##----------------------------------------------------------------------------##
## Tab: Load data.
##----------------------------------------------------------------------------##

# experiment name
output[["load_data_experiment_name"]] <- renderValueBox({
  valueBox(
    value = sample_data()$getExperiment()$experiment_name,
    subtitle = "Experiment",
    color = "light-blue"
  )
})

# number of cells
output[["load_data_number_of_cells"]] <- renderValueBox({
  valueBox(
    value = if ( !is.null(sample_data()) ) formatC(ncol(sample_data()$expression), format = "f", big.mark = ",", digits = 0) else 0,
    subtitle = "Cells",
    color = "light-blue"
  )
})

# number of grouping variables
output[["load_data_number_of_grouping_variables"]] <- renderValueBox({
  valueBox(
    value = ifelse(!is.null(sample_data()), length(sample_data()$getGroups()), 0),
    subtitle = "Grouping variables",
    color = "light-blue"
  )
})

# organism
output[["load_data_organism"]] <- renderValueBox({
  box(
    title = "Organism",
    width = 5,
    background = "light-blue",
    sample_data()$getExperiment()$organism
  )
})

# date of analysis
output[["load_data_date_of_analysis"]] <- renderValueBox({
  box(
    title = "Date when data was analyzed",
    width = 5,
    background = "light-blue",
    sample_data()$getExperiment()$date_of_analysis
  )
})

# date of export
output[["load_data_date_of_export"]] <- renderValueBox({
  box(
    title = "Date when data was exported",
    width = 5,
    background = "light-blue",
    sample_data()$getExperiment()$date_of_export
  )
})
