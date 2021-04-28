##----------------------------------------------------------------------------##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Cerebro.
##----------------------------------------------------------------------------##
output[["load_data_select_file_UI"]] <- renderUI({
  if (
    exists('Cerebro.options') &&
    !is.null(Cerebro.options[['mode']]) &&
    Cerebro.options[["mode"]] != "closed"
  ) {
    tagList(
      fluidRow(
        htmlOutput("load_data_mode_open")
      ),
      fluidRow(
        column(6,
          titlePanel("Load data"),
          fileInput(
            inputId = "input_file",
            label = "Select input data (.crb or .rds file)",
            multiple = FALSE,
            accept = c(".rds",".crb",".cerebro"),
            width = '350px',
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
          selectizeInput(
            'select_from_available_datasets', 'Available datasets',
            choices = c('Small samples (500 cells)', 'Large sample (80k cells)'),
            options = list(
              placeholder = 'Please select an option below',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        )
      )
    )
  } else {
    fluidRow(
      htmlOutput("load_data_mode_closed")
    )
  }
})

##----------------------------------------------------------------------------##
## Text message if Cerebro was launched in "open" mode.
##----------------------------------------------------------------------------##
output[["load_data_mode_open"]] <- renderText({
  if (
    exists('Cerebro.options') &&
    !is.null(Cerebro.options[["welcome_message"]])
  ) {
    HTML(Cerebro.options[["welcome_message"]])
  } else {
    HTML(
      "<h3 style='text-align: center; margin-top: 0px'><strong>Welcome to Cerebro!</strong></h3>
      <p style='text-align: center'>Please load your data set or take a look at the pre-loaded data.</p>"
    )
  }
})

##----------------------------------------------------------------------------##
## Text message if Cerebro was launched in "closed" mode.
##----------------------------------------------------------------------------##
output[["load_data_mode_closed"]] <- renderText({
  if (
    exists('Cerebro.options') &&
    !is.null(Cerebro.options[["welcome_message"]])
  ) {
    HTML(Cerebro.options[["welcome_message"]])
  } else {
    HTML(
      "<h3 style='text-align: center; margin-top: 0px'><strong>Welcome to Cerebro!</strong></h3>
      <p style='text-align: center'>Cerebro was launched in 'closed' mode, which means you cannot load your own data set. Instead, take a look at the pre-loaded data.</p>
      <br>"
    )
  }
})

observeEvent(input[['select_from_available_datasets']], {
  selected_dataset <- input[['select_from_available_datasets']]
  if (selected_dataset == 'Small samples (500 cells)') {
    data_to_load$path <- "/Users/roman/GitHub/cerebroApp_dev_refactor_overview/inst/extdata/v1.3/example.crb"
  } else if (selected_dataset == 'Large sample (80k cells)') {
    data_to_load$path <- "/Users/roman/Downloads/sc_merge_cerebro_delayed.crb"
  }
})
