##----------------------------------------------------------------------------##
## Expression by gene.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for plot.
##----------------------------------------------------------------------------##
output[["expression_by_gene_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels by gene"),
        cerebroInfoButton("expression_by_gene_info")
      ),
      plotly::plotlyOutput("expression_by_gene")
    )
  )
})

##----------------------------------------------------------------------------##
## Bar plot.
##----------------------------------------------------------------------------##
output[["expression_by_gene"]] <- plotly::renderPlotly({
  req(input[["expression_projection_color_scale"]])
  ## prepare expression levels, depending on genes provided by user
  ## ... if no genes are available
  if ( length(expression_selected_genes()$genes_to_display_present) == 0 ) {
    ## manually prepare empty data frame
    expression_levels <- data.frame(
      "gene" = character(),
      "expression" = integer()
    )
  ## ... if at least 1 gene has been provided
  } else if ( length(expression_selected_genes()$genes_to_display_present) >= 1 ) {
    ## - calculate mean expression for every gene across all cells
    ## - sort genes by mean expression from high to low
    ## - show only first 50 genes if more are available
    expression_levels <- getMeanExpressionForGenes(expression_selected_genes()$genes_to_display_present) %>%
    dplyr::slice_max(expression, n = 50)
  }
  ## prepare color scale, either "viridis" or other
  ## ...
  if ( input[["expression_projection_color_scale"]] == 'viridis' ) {
    color_scale <- 'Viridis'
  ## ...
  } else {
    color_scale <- input[["expression_projection_color_scale"]]
  }
  ## prepare plot
  plotly::plot_ly(
    expression_levels,
    x = ~gene,
    y = ~expression,
    text = ~paste0(
      expression_levels$gene, ': ',
      format(expression_levels$expression, digits = 3)
    ),
    type = "bar",
    marker = list(
      color = ~expression,
      colorscale = color_scale,
      reversescale = TRUE,
      line = list(
        color = "rgb(196,196,196)",
        width = 1
      )
    ),
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
  plotly::layout(
    title = "",
    xaxis = list(
      title = "",
      type = "category",
      categoryorder = "array",
      categoryarray = expression_levels$gene,
      mirror = TRUE,
      showline = TRUE
    ),
    yaxis = list(
      title = "Expression level",
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
observeEvent(input[["expression_by_gene_info"]], {
  showModal(
    modalDialog(
      expression_by_gene_info[["text"]],
      title = expression_by_gene_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_by_gene_info <- list(
  title = "Expression levels by gene",
  text = p("Log-normalised expression of 50 highest expressed genes inserted above. Shows mean across all cells.")
)
