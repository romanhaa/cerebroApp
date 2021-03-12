##----------------------------------------------------------------------------##
## Violin plots with plotly, e.g. for expression metrics.
##----------------------------------------------------------------------------##
plotlyViolin <- function(
  table,
  metric,
  coloring_variable,
  colors,
  y_title,
  mode
) {
  if ( mode == "percent" ) {
    y_range <- c(0,1)
    y_tickformat <- ",.0%"
    y_hoverformat <- ",.1%"
  } else if ( mode == "integer" ) {
    y_range <- NULL
    y_tickformat <- ",.0f"
    y_hoverformat <- ",.0f"
  }
  ##
  plot <- table %>%
    plotly::plot_ly(
      x = ~.[[ coloring_variable ]],
      y = ~.[[ metric ]],
      type = "violin",
      box = list(
        visible = TRUE
      ),
      meanline = list(
        visible = TRUE
      ),
      color = ~.[[ coloring_variable ]],
      colors = colors,
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
        title = y_title,
        range = y_range,
        tickformat = y_tickformat,
        hoverformat = y_hoverformat,
        mirror = TRUE,
        showline = TRUE
      ),
      dragmode = "select",
      hovermode = "compare"
    )

  ##
  return(plot)
}

##----------------------------------------------------------------------------##
## Bar chart for composition plots.
##----------------------------------------------------------------------------##
plotlyBarChart <- function(
  table,
  first_grouping_variable,
  second_grouping_variable,
  colors,
  percent
) {
  ## TODO: safety checks?
  ##
  if ( percent == FALSE ) {
    y_title <- "Number of cells"
    y_range <- NULL
    y_tickformat <- ",.0f"
    y_hoverformat <- ",.0f"
    hover_info <- glue::glue(
      "<b>{table[[ second_grouping_variable ]]}:</b> ",
      "{formatC(table[['count']], big.mark = ',')}"
    )
  ##
  } else if ( percent == TRUE ) {
    y_title <- "Percent of cells"
    y_range <- c(0,1)
    y_tickformat <- ",.0%"
    y_hoverformat <- ".1%"
    hover_info <- glue::glue(
      "<b>{table[[ second_grouping_variable ]]}:</b> ",
      "{format(round(table[['count']]*100, 1), nsmall = 1)}%"
    )
  }
  ## generate plot
  plot <- table %>%
    plotly::plot_ly(
      x = ~.[[ first_grouping_variable ]],
      y = ~count,
      type = "bar",
      color = ~.[[ second_grouping_variable ]],
      colors = colors,
      hoverinfo = "text",
      text = hover_info
    ) %>%
    plotly::layout(
      xaxis = list(
        title = "",
        mirror = TRUE,
        showline = TRUE
      ),
      yaxis = list(
        title = y_title,
        range = y_range,
        tickformat = y_tickformat,
        hoverformat = y_hoverformat,
        mirror = TRUE,
        zeroline = FALSE,
        showline = TRUE
      ),
      barmode = "stack",
      hovermode = "compare"
    )

  ##
  return(plot)
}

##----------------------------------------------------------------------------##
## Sankey plot for composition plots.
##----------------------------------------------------------------------------##
plotlySankeyPlot <- function(
  table,
  first_grouping_variable,
  second_grouping_variable,
  colors_for_groups
) {
  ## transform factor levels to integers (necessary for plotly)
  table[["source"]] <- as.numeric(table[[1]]) - 1
  table[["target"]] <- as.numeric(table[[2]]) - 1 + length(unique(table[[1]]))
  ## combine all factor levels in a single vector
  all_groups <- c(levels(table[[1]]), levels(table[[2]]))
  ## match color codes to group levels (from both groups)
  colors_for_groups_all <- colors_for_groups[names(colors_for_groups) %in% all_groups]
  ## prepare plot
  plot <- plotly::plot_ly(
      type = "sankey",
      orientation = "v",
      valueformat = ".0f",
      node = list(
        label = all_groups,
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          "%{value:,.0f} cells",
          "<extra></extra>",
          collapse = ""
        ),
        color = colors_for_groups_all,
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      link = list(
        source = table[["source"]],
        target = table[["target"]],
        value =  table[[3]],
        hoverinfo = "all",
        hovertemplate = paste0(
          "<b>", first_grouping_variable, ":</b> %{source.label}<br>",
          "<b>", second_grouping_variable, ":</b> %{target.label}<br>",
          "<b>Number of cells:</b> %{value:,.0f}",
          "<extra></extra>",
          collapse = ""
        )
      )
    )
  ##
  return(plot)
}
