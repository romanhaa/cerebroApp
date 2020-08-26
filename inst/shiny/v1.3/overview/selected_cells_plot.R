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
      tagList(
        selectInput(
          "overview_selected_cells_plot_select_variable",
          label = "Variable to compare:",
          choices = colnames(getMetaData())[! colnames(getMetaData()) %in% c("cell_barcode")]
        ),
        plotly::plotlyOutput("overview_details_selected_cells_plot")
      )
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
    input[["overview_selected_cells_plot_select_variable"]]
  )

  ## extract cells to plot
  cells_df <- cbind(
      getProjection(input[["overview_projection_to_display"]]),
      getMetaData()
    )

  ##
  ## ...
  if (
    is.null(plotly::event_data("plotly_selected", source = "overview_projection")) ||
    length(plotly::event_data("plotly_selected", source = "overview_projection")) == 0
  ) {

    ###
    cells_df <- cells_df %>% dplyr::mutate(group = 'not selected')

  ## ...
  } else {

    ##
    selected_cells <- plotly::event_data("plotly_selected", source = "overview_projection") %>%
      dplyr::mutate(identifier = paste0(x, '-', y))

    ##
    cells_df <- cells_df %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(
        identifier = paste0(X1, '-', X2),
        group = ifelse(identifier %in% selected_cells$identifier, 'selected', 'not selected'),
        group = factor(group, levels = c('selected', 'not selected'))
      )
  }

  color_variable <- input[["overview_selected_cells_plot_select_variable"]]

  ## if the selected coloring variable is categorical, represent the selected
  ## cells in a bar chart
  if (
    is.factor(cells_df[[ color_variable ]]) ||
    is.character(cells_df[[ color_variable ]])
  ) {

    ## filter table for selected cells
    cells_df <- cells_df %>%
      dplyr::filter(group == 'selected')

    ## prepare table, depending on whether at least a single cell is selected
    ## ... at least 1 cell is selected
    if ( nrow(cells_df) > 0 ) {

      ## count the number of cells by selected meta data column
      cells_df <- cells_df %>%
        dplyr::select(!!! rlang::syms(color_variable)) %>%
        dplyr::group_by_at(1) %>%
        dplyr::tally() %>%
        dplyr::ungroup()

    ## ... no cell is selected
    } else {

      ## check whether the selected meta data column contains a registered
      ## grouping variable
      ## ... the column is a grouping variable
      if ( color_variable %in% getGroups() ) {

        ## get levels for the grouping variable
        group_levels <- getGroupLevels(color_variable)

      ## ... the column is not a known grouping variable
      } else {

        ## get unique values on the meta data column
        group_levels <- unique(getMetaData()[[color_variable]])
      }

      ## create empty table to show
      cells_df <- data.frame(
          group = group_levels,
          n = 0
        ) %>%
        dplyr::rename(!!color_variable := group)
    }

    ## convert factor to character to avoid empty bars when selecting cells of
    ## certain groups
    cells_df[[1]] <- as.character(cells_df[[1]])

    ## get colors for groups
    colors_for_groups <- assignColorsToGroups(cells_df, color_variable)

    ## make bar chart
    plotly::plot_ly(
      cells_df,
      x = ~cells_df[[1]],
      y = ~cells_df[[2]],
      type = "bar",
      color = ~cells_df[[1]],
      colors = colors_for_groups,
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

  ## if the selected coloring variable is numeric/continuous
  } else if ( is.numeric(cells_df[[ color_variable ]]) ) {

    ## remove unnecessary columns
    cells_df <- cells_df %>%
      dplyr::select(group, !!! rlang::syms(color_variable))

    ## create violin/box plot
    plotly::plot_ly(
      cells_df,
      x = ~cells_df[[1]],
      y = ~cells_df[[2]],
      type = "violin",
      box = list(
        visible = TRUE
      ),
      meanline = list(
        visible = TRUE
      ),
      color = ~cells_df[[1]],
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
        title = colnames(cells_df)[2],
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
      footer = NULL,
      size = "l"
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
