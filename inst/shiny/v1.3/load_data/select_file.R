##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Cerebro.
##----------------------------------------------------------------------------##

output[["load_data_select_file_UI"]] <- renderUI({
  if (
    exists('Cerebro.options') &&
    !is.null(.GlobalEnv$Cerebro.options[['mode']]) &&
    .GlobalEnv$Cerebro.options[["mode"]] != "closed"
  ) {
    tagList(
      fluidRow(
        htmlOutput("load_data_mode_open")
      ),
      fluidRow(
        column(12,
          titlePanel("Load data"),
          fileInput(
            inputId = "input_file",
            label = "Select input data (.crb or .rds file)",
            multiple = FALSE,
            accept = c(".rds",".crb",".cerebro"),
            width = '350px',
            buttonLabel = "Browse...",
            placeholder = "No file selected"
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
  paste0(
    "<h3 style='text-align: center; margin-top: 0'><strong>Welcome to Cerebro!</strong></h2>",
    "<p style='text-align: center'>Please load your data set or take a look at the pre-loaded data.</p>"
  )
})

##----------------------------------------------------------------------------##
## Text message if Cerebro was launched in "closed" mode.
##----------------------------------------------------------------------------##

output[["load_data_mode_closed"]] <- renderText({
  paste0(
    "<h3 style='text-align: center; margin-top: 0'><strong>Welcome to Cerebro!</strong></h2>",
    "<p style='text-align: center'>Cerebro was launched in 'closed' mode, which means you cannot load your own data set. Instead, take a look at the pre-loaded data.</p>",
    "<br>"
  )
})
