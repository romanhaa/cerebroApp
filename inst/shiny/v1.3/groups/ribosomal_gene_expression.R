##----------------------------------------------------------------------------##
## Tab: Groups
##
## Expression of ribosomal genes.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["groups_ribosomal_expression_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Ribosomal gene expression"),
        cerebroInfoButton("groups_percent_ribo_info")
      ),
      uiOutput("groups_percent_ribo_UI")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["groups_percent_ribo_UI"]] <- renderUI({
  if ( "percent_ribo" %in% colnames(getMetaData()) ) {
    plotly::plotlyOutput("groups_percent_ribo_plot")
  } else {
    textOutput("groups_percent_ribo_text")
  }
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["groups_percent_ribo_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]])
  getMetaData() %>%
  plotly::plot_ly(
    x = ~.[[ input[["groups_selected_group"]] ]],
    y = ~percent_ribo*100,
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
    marker = list(size = 5)
  ) %>%
  plotly::layout(
    title = "",
    xaxis = list(
      title = "",
      mirror = TRUE,
      showline = TRUE
    ),
    yaxis = list(
      title = "Percentage of transcripts [%]",
      range = c(0,100),
      hoverformat = ".1f",
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

output[["groups_percent_ribo_text"]] <- renderText({
  "Column with percentage of ribosomal expression not available."
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["groups_percent_ribo_info"]], {
  showModal(
    modalDialog(
      groups_percent_ribo_info[["text"]],
      title = groups_percent_ribo_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

groups_percent_ribo_info <- list(
  title = "Ribosomal gene expression",
  text = p("Violin plot of the percentage of ribosomal gene expression found in each group. This reflects the contribution of ribosomal transcripts to the entire transcriptome in each cell. A list of all genes considered to be ribosomal can be found in the 'Analysis info' tab on the left.")
)
