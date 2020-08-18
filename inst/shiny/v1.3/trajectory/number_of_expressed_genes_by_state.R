##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Number of expressed gebes by state.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_nGene_by_state_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Number of expressed genes by state"),
        cerebroInfoButton("states_nGene_info")
      ),
      plotly::plotlyOutput("states_nGene_plot")
    )
  )
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["states_nGene_plot"]] <- plotly::renderPlotly({
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
  colors_this_plot <- setNames(
    default_colorset[1:length(levels(trajectory_data[["meta"]]$state))],
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
    y = ~nGene,
    type = "violin",
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    ),
    color = ~state,
    colors = colors_this_plot,
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
      title = "Number of expressed genes",
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

observeEvent(input[["states_nGene_info"]], {
  showModal(
    modalDialog(
      states_nGene_info[["text"]],
      title = states_nGene_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

states_nGene_info <- list(
  title = "Number of expressed genes by state",
  text = p("Violin plot of the number of expressed genes found in each state.")
)
