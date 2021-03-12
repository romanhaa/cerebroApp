##----------------------------------------------------------------------------##
## Expression by pseudotime.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for plot.
##----------------------------------------------------------------------------##
output[["expression_by_pseudotime_UI"]] <- renderUI({
  req(
    input[["expression_projection_to_display"]] %in% availableProjections() == FALSE
  )
  ## split selection into method and name
  selection <- strsplit(input[["expression_projection_to_display"]], split = ' // ')[[1]]
  ## check if method and name exist and don't proceed if not
  req(
    selection[1] %in% getMethodsForTrajectories(),
    selection[2] %in% getNamesOfTrajectories(selection[1])
  )
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels by pseudotime"),
        cerebroInfoButton("expression_by_pseudotime_info"),
        shinyWidgets::dropdownButton(
          tags$div(
            tags$style(
              HTML("div.awesome-checkbox {margin-top: 10px;}")
            ),
            style = "color: black !important;",
            tagList(
              shinyWidgets::awesomeCheckbox(
                inputId = "expression_by_pseudotime_show_trend_line",
                label = HTML("Show trend line"),
                value = TRUE
              ),
              sliderInput(
                "expression_by_pseudotime_trend_line_bandwidth",
                label = "Bandwidth for trend line",
                min = 1,
                max = 50,
                step = 1,
                value = 7
              ),
              sliderInput(
                "expression_by_pseudotime_trend_line_width",
                label = "Width of line",
                min = 1,
                max = 10,
                step = 1,
                value = 2
              )
            )
          ),
          circle = FALSE,
          icon = icon("cog"),
          inline = TRUE,
          size = "xs"
        )
      ),
      plotly::plotlyOutput("expression_by_pseudotime")
    )
  )
})

##----------------------------------------------------------------------------##
## Plot.
##----------------------------------------------------------------------------##
output[["expression_by_pseudotime"]] <- plotly::renderPlotly({
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_point_size"]],
    input[["expression_projection_point_opacity"]],
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_range"]],
    !is.null(input[["expression_by_pseudotime_show_trend_line"]]),
    input[["expression_by_pseudotime_trend_line_bandwidth"]],
    input[["expression_by_pseudotime_trend_line_width"]],
    expression_projection_data(),
    "pseudotime" %in% colnames(expression_projection_data())
  )
  cells_df <- expression_projection_data()
  ## prepare hover info
  hover_info <- buildHoverInfoForProjections(cells_df)
  ## add expression levels to hover info
  hover_info <- glue::glue(
    "{hover_info}
    <b>Pseudotime</b>: {formatC(cells_df[[ 'pseudotime' ]], format = 'f', digits = 2)}
    <b>State</b>: {cells_df[[ 'state' ]]}"
  )
  ## check selected color scale
  ## ... selected color scale is "viridis"
  if ( input[["expression_projection_color_scale"]] == 'viridis' ) {
    color_scale <- 'Viridis'
  ## ... selected color scale is anything else than "viridis"
  } else {
    color_scale <- input[["expression_projection_color_scale"]]
  }
  ## prepare plot
  plot <- plotly::plot_ly() %>%
    plotly::add_trace(
      data = cells_df,
      x = ~pseudotime,
      y = ~level,
      type = "scatter",
      mode = "markers",
      marker = list(
        colorbar = list(
          title = "Expression"
        ),
        color = ~level,
        opacity = input[["expression_projection_point_opacity"]],
        colorscale = color_scale,
        cauto = FALSE,
        cmin = input[["expression_projection_color_range"]][1],
        cmax = input[["expression_projection_color_range"]][2],
        reversescale = TRUE,
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["expression_projection_point_size"]],
        showscale = FALSE
      ),
      hoverinfo = "text",
      text = ~hover_info
    ) %>%
    plotly::layout(
      xaxis = list(
        title = "Pseudotime",
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Expression level",
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE
      ),
      hoverlabel = list(
        font = list(
          size = 11,
          color = "black"
        ),
        bgcolor = "lightgrey"
      )
    )
  ## add trend line if activated
  if ( input[["expression_by_pseudotime_show_trend_line"]] == TRUE ) {
    ## calculate smoothened trend line
    trend_line = stats::ksmooth(
      cells_df$pseudotime,
      cells_df$level,
      "normal",
      input[["expression_by_pseudotime_trend_line_bandwidth"]],
      x.points = cells_df$pseudotime
    )
    ## add trend line to plot
    plot <- plotly::add_trace(
      plot,
      x = trend_line$x,
      y = trend_line$y,
      type = "scatter",
      mode = "lines",
      line = list(
        dash = "solid",
        width = input[["expression_by_pseudotime_trend_line_width"]],
        color = "#e74c3c"
      ),
      name = "Trend line",
      hoverinfo = "text",
      text = ~glue::glue(
        "<b>Trend line</b>
        <b>Pseudotime:</b> {formatC(trend_line$x, format = 'f', digits = 2)}
        <b>Expression:</b> {formatC(trend_line$y, format = 'f', digits = 3)}"
      ),
      showlegend = FALSE
    )
  }
  ## if set in options, return plot with WebGL
  if ( preferences$use_webgl == TRUE ) {
    plotly::toWebGL(plot)
  } else {
    plot
  }
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_by_pseudotime_info"]], {
  showModal(
    modalDialog(
      expression_by_pseudotime_info$text,
      title = expression_by_pseudotime_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_by_pseudotime_info <- list(
  title = "Expression levels by pseudotime",
  text = HTML("
    Shown in this plot are the expression levels along pseudotime. This can help to judge whether genes or gene sets might play a role in the process represented by the trajectory. By default, a trend line will be added. It can be deactivated using the checkbox in the dropdown menu behind the gear icon in the title of the panel. There, you will also find elements to control the line width and the bandwidth parameter, which determines how local the trend line should be. Lower bandwidth values will results in a more local curve than larger values.
    <em>This panel is only visible when a trajectory is selected that contains a 'pseudotime' column.</em><br>
  ")
)
