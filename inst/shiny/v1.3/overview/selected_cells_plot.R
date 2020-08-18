##----------------------------------------------------------------------------##
## Tab: Overview
##
## Plot of selected cells.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["overview_selected_cells_plot_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Plot of selected cells"),
        cerebroInfoButton("overview_details_selected_cells_plot_info")
      ),
      plotly::plotlyOutput("overview_details_selected_cells_plot")
    )
  )
})

##----------------------------------------------------------------------------##
## Plot for selected cells.
## - in sync with selected color variable
##   - if categorical: number of cells in each group
##   - if numerical: box/violin plot
##----------------------------------------------------------------------------##

output[["overview_details_selected_cells_plot"]] <- plotly::renderPlotly({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_point_color"]]
  )

  ## extract cells to plot
  to_plot <- cbind(
      getProjection(input[["overview_projection_to_display"]]),
      getMetaData()
    ) %>% 
    as.data.frame()

  if (
    is.null(plotly::event_data("plotly_selected", source = "overview_projection")) ||
    length(plotly::event_data("plotly_selected", source = "overview_projection")) == 0
  ) {
    to_plot <- to_plot %>% dplyr::mutate(group = 'not selected')
  } else {
    selected_cells <- plotly::event_data("plotly_selected", source = "overview_projection") %>%
      dplyr::mutate(identifier = paste0(x, '-', y))
    to_plot <- to_plot %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(
        identifier = paste0(X1, '-', X2),
        group = ifelse(identifier %in% selected_cells$identifier, 'selected', 'not selected'),
        group = factor(group, levels = c('selected', 'not selected'))
      )
  }

  color_variable <- input[["overview_point_color"]]

  ## if the selected coloring variable is categorical, represent the selected
  ## cells in a bar chart
  if (
    is.factor(to_plot[[ color_variable ]]) ||
    is.character(to_plot[[ color_variable ]])
  ) {

    ## calculate number of cells in each group
    t <- to_plot %>%
      dplyr::filter(group == 'selected')

    ## prepare table, depending on whether at least a single cell is selected
    ## ... at least 1 cell is selected
    if ( nrow(t) > 0 ) {

      ## count the number of cells by selected meta data column
      t <- t %>%
        dplyr::select(!!! rlang::syms(color_variable)) %>%
        dplyr::group_by_at(1) %>%
        dplyr::tally() %>%
        dplyr::ungroup()

    ## ... no cell is selected
    } else {

      ## check whether the selected meta data column contains a registered
      ## grouping variable
      ## ... the column is a grouping variable
      if ( input[["overview_point_color"]] %in% getGroups() ) {

        ## get levels for the grouping variable
        group_levels <- getGroupLevels(input[["overview_point_color"]])

      ## ... the column is not a known grouping variable
      } else {

        ## get unique values on the meta data column
        group_levels <- unique(getMetaData()[[input[["overview_point_color"]]]])
      }

      ## create empty table to show
      t <- data.frame(
          group = group_levels,
          n = 0
        ) %>%
        dplyr::rename(!!input[["overview_point_color"]] := group)

    }

    ## convert factor to character to avoid empty bars when selecting cells of
    ## certain groups
    t[[1]] <- as.character(t[[1]])

    ## create color assignment for groups
    if ( input[["overview_point_color"]] %in% getGroups() ) {
      colors_this_plot <- reactive_colors()[[ input[["overview_point_color"]] ]]
    } else if ( input[["overview_point_color"]] %in% getCellCycle() ) {
      colors_this_plot <- reactive_colors()[[ input[["overview_point_color"]] ]]
    } else {
      colors_this_plot <- setNames(
        default_colorset[1:length(t[[ 1 ]])],
        t[[ 1 ]]
      )
    }

    ## make bar chart
    plotly::plot_ly(
      t,
      x = ~t[[1]],
      y = ~t[[2]],
      type = "bar",
      color = ~t[[1]],
      colors = colors_this_plot,
      source = "subset",
      showlegend = FALSE,
      hoverinfo = "y"
    ) %>%
    plotly::layout(
      title = "",
      xaxis = list(
        title = "",
        mirror = TRUE,
        showline = TRUE
      ),
      yaxis = list(
        title = "Number of cells",
        hoverformat = ".0f",
        mirror = TRUE,
        showline = TRUE
      ),
      dragmode = "select",
      hovermode = "compare"
    )

  ## if the selected coloring variable is not categorical but continuous
  } else {
    ## remove unnecessary columns
    t <- to_plot %>%
      dplyr::select(group, !!! rlang::syms(color_variable))

    ## create violin/box plot
    plotly::plot_ly(
      t,
      x = ~t[[1]],
      y = ~t[[2]],
      type = "violin",
      box = list(
        visible = TRUE
      ),
      meanline = list(
        visible = TRUE
      ),
      color = ~t[[1]],
      colors = setNames(c('#e74c3c','#7f8c8d'), c('selected', 'not selected')),
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
        title = colnames(t)[2],
        hoverformat = ".0f",
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

observeEvent(input[["overview_details_selected_cells_plot_info"]], {
  showModal(
    modalDialog(
      overview_details_selected_cells_plot_info$text,
      title = overview_details_selected_cells_plot_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

overview_details_selected_cells_plot_info <- list(
  title = "Plot of selected cells",
  text = p("Depending on the variable selected to color cells in the dimensional reduction, this plot will show different things. If you select a categorical variable, e.g. 'sample' or 'cluster', you will get a bar plot showing which groups the cells selected with the box or lasso tool come from. Instead, if you select a continuous variable, e.g. the number of transcripts (nUMI), you will see a violin/box plot showing the distribution of that variable in the selected vs. non-selected cells.")
)
