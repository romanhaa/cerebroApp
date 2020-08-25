##----------------------------------------------------------------------------##
## Tab: Groups
##
## Expression of mitochondrial genes.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["groups_mitochondrial_expression_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Mitochondrial gene expression"),
        cerebroInfoButton("groups_percent_mt_info")
      ),
      uiOutput("groups_percent_mt_UI")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["groups_percent_mt_UI"]] <- renderUI({
  if ( "percent_mt" %in% colnames(getMetaData()) ) {
    plotly::plotlyOutput("groups_percent_mt_plot")
  } else {
    textOutput("groups_percent_mt_text")
  }
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["groups_percent_mt_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]])
  getMetaData() %>%
  plotly::plot_ly(
    x = ~.[[ input[["groups_selected_group"]] ]],
    y = ~percent_mt*100,
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

output[["groups_percent_mt_text"]] <- renderText({
  "Column with percentage of mitochondrial expression not available."
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["groups_percent_mt_info"]], {
  showModal(
    modalDialog(
      groups_percent_mt_info[["text"]],
      title = groups_percent_mt_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

groups_percent_mt_info <- list(
  title = "Mitochondrial gene expression",
  text = p("Violin plot of the percentage of mitochondrial gene expression found in each group. This reflects the contribution of mitochondrial transcripts to the entire transcriptome in each cell. A list of all genes considered to be mitochondrial can be found in the 'Analysis info' tab on the left.")
)
