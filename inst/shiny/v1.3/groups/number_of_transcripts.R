##----------------------------------------------------------------------------##
## Tab: Groups
##
## Number of transcripts.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["groups_number_of_transcripts_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Number of transcripts"),
        cerebroInfoButton("groups_nUMI_info")
      ),
      uiOutput("groups_nUMI_UI")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["groups_nUMI_UI"]] <- renderUI({
  if ( "nUMI" %in% colnames(getMetaData()) ) {
    plotly::plotlyOutput("groups_nUMI_plot")
  } else {
    textOutput("groups_nUMI_text")
  }
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["groups_nUMI_plot"]] <- plotly::renderPlotly({
  req(
    input[["groups_selected_group"]]
  )
  getMetaData() %>%
  plotly::plot_ly(
    x = ~.[[ input[["groups_selected_group"]] ]],
    y = ~nUMI,
    type = "violin",
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    ),
    color = ~.[[ input[["groups_selected_group"]] ]],
    colors = reactive_colors()[[ input[["groups_selected_group"]] ]],
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
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##

output[["groups_nUMI_text"]] <- renderText({
  "Column with number of transcript per cell not available."
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["groups_nUMI_info"]], {
  showModal(
    modalDialog(
      groups_nUMI_info[["text"]],
      title = groups_nUMI_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

groups_nUMI_info <- list(
  title = "Number of transcripts",
  text = p("Violin plot of the number of transcripts (UMIs) found in each group.")
)
