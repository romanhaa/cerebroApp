##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Select method and name.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to set layout for selection of method and name, which are split
## because the names of available trajectories depends on which method is
## selected. If no method is available, show message that data is missing.
##----------------------------------------------------------------------------##

output[["trajectory_select_method_and_name_UI"]] <- renderUI({
  if ( length(getMethodsForTrajectories()) == 0 ) {
    fluidRow(
      cerebroBox(
        title = "Trajectory",
        textOutput("trajectory_missing")
      )
    )
  } else if ( length(getMethodsForTrajectories()) > 0 ) {
    tagList(
      fluidRow(
        column(
          6,
          uiOutput("trajectory_selected_method_UI")
        ),
        column(
          6,
          uiOutput("trajectory_selected_name_UI")
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
## UI element to select from which method the results should be shown.
##----------------------------------------------------------------------------##

output[["trajectory_selected_method_UI"]] <- renderUI({
  tagList(
    div(
      HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a method:</strong></h2>')
    ),
    fluidRow(
      column(2),
      column(8,
        selectInput(
          "trajectory_selected_method",
          label = NULL,
          choices = getMethodsForTrajectories(),
          width = "100%"
        )
      ),
      column(2)
    )
  )
})

##----------------------------------------------------------------------------##
## UI element to select which trajectory (name) should be shown.
##----------------------------------------------------------------------------##

output[["trajectory_selected_name_UI"]] <- renderUI({
  req(
    input[["trajectory_selected_method"]]
  )
  tagList(
    div(
      HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a trajectory:</strong></h2>')
    ),
    fluidRow(
      column(2),
      column(8,
        selectInput(
          "trajectory_selected_name",
          label = NULL,
          choices = getNamesOfTrajectories(input[["trajectory_selected_method"]]),
          width = "100%"
        )
      ),
      column(2)
    )
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##

output[["trajectory_missing"]] <- renderText({
  "No trajectories available to display."
})
