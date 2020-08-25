##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Number of transcripts by state.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_nUMI_by_state_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Number of transcripts by state"),
        cerebroInfoButton("states_nUMI_info")
      ),
      plotly::plotlyOutput("states_nUMI_plot")
    )
  )
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["states_nUMI_plot"]] <- plotly::renderPlotly({

  ##
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  ## collect trajectory data
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
  
  ##
  state_colors <- setNames(
    default_colorset[seq_along(levels(trajectory_data[["meta"]]$state))],
    levels(trajectory_data[["meta"]]$state)
  )

  ##
  cbind(
    trajectory_data[["meta"]],
    getMetaData()
  ) %>%
  dplyr::filter(!is.na(pseudotime)) %>%
  plotly::plot_ly(
    x = ~state,
    y = ~nUMI,
    type = "violin",
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    ),
    color = ~state,
    colors = state_colors,
    source = "subset",
    showlegend = FALSE,
    hoverinfo = "y",
    marker = list(
      size = 5
    )
  ) %>%
  plotly::layout(
    title = "",
    xaxis = list(
      title = "",
      mirror = TRUE,
      showline = TRUE
    ),
    yaxis = list(
      title = "Number of transcripts",
      hoverformat = ".0f",
      mirror = TRUE,
      showline = TRUE
    ),
    dragmode = "select",
    hovermode = "compare"
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["states_nUMI_info"]], {
  showModal(
    modalDialog(
      states_nUMI_info[["text"]],
      title = states_nUMI_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

states_nUMI_info <- list(
  title = "Number of transcripts by state",
  text = p("Violin plot of the number of transcripts (UMIs) found in each state.")
)