##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##
## Expression in selected cells.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for plot.
##----------------------------------------------------------------------------##

output[["expression_in_selected_cells_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels in selected cells"),
        cerebroInfoButton("expression_in_selected_cells_info")
      ),
      plotly::plotlyOutput("expression_in_selected_cells")
    )
  )
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["expression_in_selected_cells"]] <- plotly::renderPlotly({

  ## don't proceed without these inputs
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_point_size"]],
    input[["expression_projection_point_opacity"]],
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_scale_range"]],
    input[["expression_projection_scale_x_manual_range"]],
    input[["expression_projection_scale_y_manual_range"]],
    gene_expression_plot_data()
  )

  ## check if user requested to show expression in separate panels
  ## ... separate panels requested and "gene" column present (which means
  ##     expression was actually split by gene)
  if (
    input[["expression_projection_show_genes_in_separate_panels"]] == TRUE &&
    "gene" %in% colnames(gene_expression_plot_data()) == TRUE
  ) {

    ## don't plot anything because data is not present
    ## even if I merged all meta data in the data frame, it wouldn't be correct
    ## because cells are plotted once per gene

  ## ...
  } else {

    ## prepare data to be plotted
    ## ... if no selection was made or no cells are in selection
    if (
      is.null(plotly::event_data("plotly_selected", source = "expression_projection")) |
      length(plotly::event_data("plotly_selected", source = "expression_projection")) == 0
    ) {

      ## assign all cells to "not selected" group
      cells_df <- gene_expression_plot_data() %>%
        dplyr::mutate(group = 'not selected')

    ## ... if at least 1 cell was selected
    } else {

      ## get names of selected cells
      selected_cells <- plotly::event_data("plotly_selected", source = "expression_projection") %>%
        dplyr::mutate(identifier = paste0(x, '-', y))

      ## - get data to plot
      ## - assign cells to either "selected" or "not selected" based on their name
      ## - keep only relevant columns
      cells_df <- gene_expression_plot_data() %>%
        dplyr::rename(X1 = 1, X2 = 2) %>%
        dplyr::mutate(
          identifier = paste0(X1, '-', X2),
          group = ifelse(identifier %in% selected_cells$identifier, 'selected', 'not selected'),
          group = factor(group, levels = c('selected', 'not selected'))
        ) %>%
        dplyr::select(group, level)
    }

    ## prepare plot
    plotly::plot_ly(
      cells_df,
      x = ~group,
      y = ~level,
      type = "violin",
      box = list(
        visible = TRUE
      ),
      meanline = list(
        visible = TRUE
      ),
      color = ~group,
      colors = setNames(c('#e74c3c','#7f8c8d'),c('selected', 'not selected')),
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
        range = c(0, max(cells_df$level, na.rm = TRUE) * 1.2),
        hoverformat = ".2f",
        mirror = TRUE,
        showline = TRUE
      ),
      dragmode = "select",
      hovermode = "compare"
    )
  }
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["expression_in_selected_cells_info"]], {
  showModal(
    modalDialog(
      expression_in_selected_cells_info$text,
      title = expression_in_selected_cells_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

expression_in_selected_cells_info <- list(
  title = "Expression levels in selected cells",
  text = p("This plot shows the log-normalised expression of selected genes for cells grouped by whether they were selected using the box or lasso selection tool. If more than 1 gene was provided, this reflects the average across all cells of each sample.")
)
