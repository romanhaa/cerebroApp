##----------------------------------------------------------------------------##
## Plot for selected cells.
## - in sync with selected color variable
##   - if categorical: number of cells in each group
##   - if numerical: box/violin plot
##----------------------------------------------------------------------------##
output[["overview_details_selected_cells_plot"]] <- plotly::renderPlotly({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_to_display"]] %in% availableProjections(),
    input[["overview_selected_cells_plot_select_variable"]],
    overview_projection_selected_cells()
  )
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if ( nrow(overview_projection_selected_cells())==0 ) {
    ###
    cells_df <- getMetaData() %>%
      dplyr::mutate(group = 'not selected')
  ## ... selection has been made and at least 1 cell is in it
  } else {
    selected_cells <- overview_projection_selected_cells()$cell_barcode
    ##
    cells_df <- getMetaData() %>%
      dplyr::mutate(
        group = ifelse(cell_barcode %in% selected_cells, 'selected', 'not selected'),
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
        dplyr::group_by(dplyr::across(c(color_variable))) %>%
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
    plot <- plotly::plot_ly(
        cells_df,
        x = ~cells_df[[1]],
        y = ~cells_df[[2]],
        type = "bar",
        color = ~cells_df[[1]],
        colors = colors_for_groups,
        source = "subset",
        showlegend = FALSE,
        hoverinfo = "y"
      )
    y_axis_title <- "Number of cells"
  ## if the selected coloring variable is numeric/continuous
  } else if ( is.numeric(cells_df[[ color_variable ]]) ) {
    ## remove unnecessary columns
    cells_df <- cells_df %>%
      dplyr::select(group, tidyselect::all_of(color_variable))
    ## create violin/box plot
    plot <- plotly::plot_ly(
        cells_df,
        x = ~cells_df[[ 'group' ]],
        y = ~cells_df[[ color_variable ]],
        type = "violin",
        box = list(
          visible = TRUE
        ),
        meanline = list(
          visible = TRUE
        ),
        color = ~cells_df[[1]],
        colors = setNames(
          c('#e74c3c','#7f8c8d'),
          c('selected', 'not selected')
        ),
        source = "subset",
        showlegend = FALSE,
        hoverinfo = "y",
        marker = list(
          size = 5
        )
      )
    y_axis_title <- colnames(cells_df)[2]
  }
  plot %>%
  plotly::layout(
    title = "",
    xaxis = list(
      title = "",
      mirror = TRUE,
      showline = TRUE
    ),
    yaxis = list(
      title = y_axis_title,
      tickformat = ",.0f",
      hoverformat = ",.0f",
      mirror = TRUE,
      showline = TRUE
    ),
    dragmode = "select",
    hovermode = "compare"
  )
})
