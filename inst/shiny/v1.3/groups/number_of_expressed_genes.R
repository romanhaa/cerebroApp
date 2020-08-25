##----------------------------------------------------------------------------##
## Tab: Groups
##
## Number of expressed genes.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["groups_number_of_expressed_genes_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Number of expressed genes"),
        cerebroInfoButton("groups_nGene_info")
      ),
      uiOutput("groups_nGene_UI")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows a plot or a text message if data is not
## available.
##----------------------------------------------------------------------------##

output[["groups_nGene_UI"]] <- renderUI({
  if ( "nGene" %in% colnames(getMetaData()) ) {
    plotly::plotlyOutput("groups_nGene_plot")
  } else {
    textOutput("groups_nGene_text")
  }
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["groups_nGene_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]])
  getMetaData() %>%
  plotly::plot_ly(
    x = ~.[[ input[["groups_selected_group"]] ]],
    y = ~nGene,
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
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##

output[["groups_nGene_text"]] <- renderText({
  "Column with number of expressed genes per cell not available."
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["groups_nGene_info"]], {
  showModal(
    modalDialog(
      groups_nGene_info[["text"]],
      title = groups_nGene_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

groups_nGene_info <- list(
  title = "Number of expressed genes",
  text = p("Violin plot of the number of expressed genes found in each group.")
)
