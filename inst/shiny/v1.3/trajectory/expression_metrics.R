##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Expression metrics:
## - number of transcripts
## - number of expressed genes
## - percent of transcripts from mitochondrial genes
## - percent of transcripts from ribosomal genes
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_expression_metrics_UI"]] <- renderUI({

  ##
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  ##
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression metrics"),
        cerebroInfoButton("trajectory_expression_metrics_info")
      ),
      tabBox(
        title = NULL,
        width = 12,
        id = "trajectory_expression_metrics_tabs",
        tabPanel(
          "Number of transcripts",
          uiOutput("trajectory_states_nUMI_UI")
        ),
        tabPanel(
          "Number of expressed genes",
          uiOutput("trajectory_states_nGene_UI")
        ),
        tabPanel(
          "Mitochondrial gene expression",
          uiOutput("trajectory_states_percent_mt_UI")
        ),
        tabPanel(
          "Ribosomal gene expression",
          uiOutput("trajectory_states_percent_ribo_UI")
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Number of transcripts.
##----------------------------------------------------------------------------##

output[["trajectory_states_nUMI_UI"]] <- renderUI({
  if ( "nUMI" %in% colnames(getMetaData()) ) {
    plotly::plotlyOutput("trajectory_states_nUMI_plot")
  } else {
    textOutput("trajectory_states_nUMI_text")
  }
})

output[["trajectory_states_nUMI_text"]] <- renderText({
  "Column with number of transcript per cell not available."
})

output[["trajectory_states_nUMI_plot"]] <- plotly::renderPlotly({

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
  trajectory_data <- trajectory_data[["meta"]]

  ##
  state_colors <- setNames(
    default_colorset[seq_along(levels(trajectory_data$state))],
    levels(trajectory_data$state)
  )

  ##
  plotlyViolin(
    table = cbind(trajectory_data, getMetaData()),
    metric = "nUMI",
    coloring_variable = "state",
    colors = state_colors,
    y_title = "Number of transcripts",
    mode = "integer"
  )
})

##----------------------------------------------------------------------------##
## Number of expressed genes.
##----------------------------------------------------------------------------##

output[["trajectory_states_nGene_UI"]] <- renderUI({
  if ( "nGene" %in% colnames(getMetaData()) ) {
    plotly::plotlyOutput("trajectory_states_nGene_plot")
  } else {
    textOutput("trajectory_states_nGene_text")
  }
})

output[["trajectory_states_nGene_text"]] <- renderText({
  "Column with number of expressed genes per cell not available."
})

output[["trajectory_states_nGene_plot"]] <- plotly::renderPlotly({

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
  trajectory_data <- trajectory_data[["meta"]]

  ##
  state_colors <- setNames(
    default_colorset[seq_along(levels(trajectory_data$state))],
    levels(trajectory_data$state)
  )

  ##
  plotlyViolin(
    table = cbind(trajectory_data, getMetaData()),
    metric = "nGene",
    coloring_variable = "state",
    colors = state_colors,
    y_title = "Number of expressed genes",
    mode = "integer"
  )
})

##----------------------------------------------------------------------------##
## Expression from mitochondrial genes.
##----------------------------------------------------------------------------##

output[["trajectory_states_percent_mt_UI"]] <- renderUI({
  if ( "percent_mt" %in% colnames(getMetaData()) ) {
    plotly::plotlyOutput("trajectory_states_percent_mt_plot")
  } else {
    textOutput("trajectory_states_percent_mt_text")
  }
})

output[["trajectory_states_percent_mt_text"]] <- renderText({
  "Column with percentage of mitochondrial expression not available."
})

output[["trajectory_states_percent_mt_plot"]] <- plotly::renderPlotly({

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
  trajectory_data <- trajectory_data[["meta"]]

  ##
  state_colors <- setNames(
    default_colorset[seq_along(levels(trajectory_data$state))],
    levels(trajectory_data$state)
  )

  ##
  plotlyViolin(
    table = cbind(trajectory_data, getMetaData()),
    metric = "percent_mt",
    coloring_variable = "state",
    colors = state_colors,
    y_title = "Percentage of transcripts",
    mode = "percent"
  )
})

##----------------------------------------------------------------------------##
## Expression from ribosomal genes.
##----------------------------------------------------------------------------##

output[["trajectory_states_percent_ribo_UI"]] <- renderUI({
  if ( "percent_ribo" %in% colnames(getMetaData()) ) {
    plotly::plotlyOutput("trajectory_states_percent_ribo_plot")
  } else {
    textOutput("trajectory_states_percent_ribo_text")
  }
})

output[["trajectory_states_percent_ribo_text"]] <- renderText({
  "Column with percentage of ribosomal expression not available."
})

output[["trajectory_states_percent_ribo_plot"]] <- plotly::renderPlotly({

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
  trajectory_data <- trajectory_data[["meta"]]

  ##
  state_colors <- setNames(
    default_colorset[seq_along(levels(trajectory_data$state))],
    levels(trajectory_data$state)
  )

  ##
  plotlyViolin(
    table = cbind(trajectory_data, getMetaData()),
    metric = "percent_ribo",
    coloring_variable = "state",
    colors = state_colors,
    y_title = "Percentage of transcripts",
    mode = "percent"
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_expression_metrics_info"]], {
  showModal(
    modalDialog(
      trajectory_expression_metrics_info[["text"]],
      title = trajectory_expression_metrics_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_expression_metrics_info <- list(
  title = "Number of transcripts",
  text = HTML("Violin plots showing the number of transcripts (nUMI/nCounts), the number of expressed genes (nGene/nFeature), as well as the percentage of transcripts coming from mitochondrial and ribosomal genes in each state.")
)
