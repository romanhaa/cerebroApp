##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##
## Expression by group
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with input selection (which group to show) and plot.
##----------------------------------------------------------------------------##

output[["expression_by_group_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels by group"),
        cerebroInfoButton("expression_by_group_info")
      ),
      tagList(
        selectInput(
          "expression_by_group_selected_group",
          label = "Select a group to show expression by:",
          choices = getGroups(),
          width = "100%"
        ),
        plotly::plotlyOutput("expression_by_group")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["expression_by_group"]] <- plotly::renderPlotly({

  ## don't proceed without these inputs
  req(
    input[["expression_projection_to_display"]],
    input[["expression_by_group_selected_group"]]
  )

  ## prepare plot
  gene_expression_plot_data() %>%
  plotly::plot_ly(
    x = ~.[[ input[["expression_by_group_selected_group"]] ]],
    y = ~level,
    type = "violin",
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    ),
    color = ~.[[ input[["expression_by_group_selected_group"]] ]],
    colors = reactive_colors()[[ input[["expression_by_group_selected_group"]] ]],
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
      title = "Expression level",
      range = c(0, max(gene_expression_plot_data()$level) * 1.2),
      hoverformat = ".2f",
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

observeEvent(input[["expression_by_group_info"]], {
  showModal(
    modalDialog(
      expression_by_group_info$text,
      title = expression_by_group_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

expression_by_group_info <- list(
  title = "Expression levels by group",
  text = p("Log-normalised expression of genes inserted above by group If more than 1 gene was provided, this reflects the average across all cells of each group")
)