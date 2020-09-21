##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Distribution along pseudotime.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_distribution_along_pseudotime_UI"]] <- renderUI({

  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Distribution along pseudotime"),
        cerebroInfoButton(
          "trajectory_distribution_along_pseudotime_info",
          style = "margin-right: 3px"
        ),
        shinyWidgets::dropdownButton(
          tags$div(
            style = "color: black !important;",
            class = "pull-right",
            tagList(
              sliderInput(
                "trajectory_distribution_along_pseudotime_opacity",
                label = "Opacity of density curve",
                min = 0,
                max = 1,
                step = 0.1,
                value = 0.6
              ),
              hr(),
              shinyWidgets::awesomeCheckbox(
                inputId = "trajectory_distribution_along_pseudotime_show_trend_line",
                label = "Show trend line",
                value = TRUE
              ),
              sliderInput(
                "trajectory_distribution_along_pseudotime_trend_line_bandwidth",
                label = "Bandwidth for trend line",
                min = 1,
                max = 50,
                step = 1,
                value = 7
              ),
              sliderInput(
                "trajectory_distribution_along_pseudotime_trend_line_width",
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
      tagList(
        selectInput(
          "trajectory_distribution_along_pseudotime_select_variable",
          label = "Variable to show along pseudotime:",
          choices = c(
            "state",
            colnames(getMetaData())[! colnames(getMetaData()) %in% c("cell_barcode")]
          )
        ),
        plotly::plotlyOutput("trajectory_distribution_along_pseudotime_plot")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Plot.
##----------------------------------------------------------------------------##

output[["trajectory_distribution_along_pseudotime_plot"]] <- plotly::renderPlotly({

  ## don't do anything before these inputs are selected
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]],
    input[["trajectory_distribution_along_pseudotime_select_variable"]],
    input[["trajectory_distribution_along_pseudotime_opacity"]],
    input[["trajectory_distribution_along_pseudotime_show_trend_line"]],
    input[["trajectory_distribution_along_pseudotime_trend_line_bandwidth"]],
    input[["trajectory_distribution_along_pseudotime_trend_line_width"]]
  )

  ## collect trajectory data
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
  
  ## extract cells to plot
  cells_df <- cbind(trajectory_data[["meta"]], getMetaData()) %>%
    dplyr::filter(!is.na(pseudotime))

  ## grab column name for cell coloring
  color_variable <- input[["trajectory_distribution_along_pseudotime_select_variable"]]

  ## ... cells are colored by a categorical variable; the Y axis will show the
  ##     density of the group along pseudotime
  if (
    is.factor(cells_df[[ color_variable ]]) ||
    is.character(cells_df[[ color_variable ]])
  ) {

    ## get colors for groups
    colors_for_groups <- assignColorsToGroups(cells_df, color_variable)

    ## get group levels
    if ( is.factor(cells_df[[ color_variable ]]) ) {
      group_levels <- levels(cells_df[[ color_variable ]])
    } else if ( is.character(cells_df[[ color_variable ]]) ) {
      group_levels <- unique(cells_df[[ color_variable ]])
    }

    ## create empty plot
    plot <- plotly::plot_ly()

    ## add trace to plot for every group level
    for ( i in seq_along(group_levels) ) {

      ## get name of current group level
      current_group <- group_levels[i]

      ## filter cells for those that are in current group
      temp_data <- cells_df[which(cells_df[[ color_variable ]] == current_group),]

      ## only proceed if at least 2 cells are in this subgroup
      if ( nrow(temp_data) > 1) {

        ## calculate density over pseudotime
        temp_density <- stats::density(temp_data[["pseudotime"]], kernel = "gaussian")

        ## add alpha value to hex colors
        temp_color <- grDevices::col2rgb(colors_for_groups[i])
        temp_color <- grDevices::rgb(
          red = temp_color[1],
          green = temp_color[2],
          blue = temp_color[3],
          alpha = input[["trajectory_distribution_along_pseudotime_opacity"]]/1*255,
          maxColorValue = 255
        )

        ## add trace to plot
        plot <- plot %>%
          plotly::add_trace(
            x = temp_density$x,
            y = temp_density$y,
            name = current_group,
            type = 'scatter',
            mode = 'lines',
            fill = 'tozeroy',
            fillcolor = temp_color,
            line = list(
              width = 0.5,
              color = colors_for_groups[i]
            ),
            hoverinfo = 'text',
            text = paste0(
              "<b>State ", current_group, "</b><br>",
              "<b>Pseudotime</b>: ", formatC(temp_density$x, format = "f", big.mark = ",", digits = 2), "<br>",
              "<b>Density</b>: ", formatC(temp_density$y, format = "f", big.mark = ",", digits = 2), "<br>"
            )
          )
      }
    }

    ## add layout to plot
    plot <- plot %>%
      plotly::layout(
        xaxis = list(
          title = "Pseudotime",
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = range(cells_df[["pseudotime"]])
        ),
        yaxis = list(
          title = "Density",
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        hovermode = "compare",
        hoverlabel = list(
          align = 'left'
        )
      )

  ## ... cells should be colored by a numeric variable; instead of showing the
  ##     density by pseudotime, the numeric variable will be shown directly on
  ##     the Y axis
  } else {

    ## get colors for states
    colors_for_groups <- assignColorsToGroups(trajectory_data[["meta"]], "state")

    ## prepare hover info
    hover_info <- buildHoverInfoForProjections(cells_df)

    ## add expression levels to hover info
    hover_info <- paste0(
      hover_info, "<br>",
      "<b>Pseudotime</b>: ", formatC(cells_df[[ "pseudotime" ]], format = "f", big.mark = ",", digits = 2), "<br>",
      "<b>State</b>: ", cells_df[[ "state" ]], "<br>"
    )

    ## prepare plot
    plot <- plotly::plot_ly() %>%
      plotly::add_trace(
        data = cells_df,
        x = ~pseudotime,
        y = ~cells_df[[ color_variable ]],
        type = "scatter",
        mode = "markers",
        color = ~state,
        colors = colors_for_groups,
        marker = list(
          opacity = input[["trajectory_point_opacity"]],
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["trajectory_point_size"]]
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
          title = color_variable,
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        hoverlabel = list(
          font = list(
            size = 11,
            color = "black"
          ),
          align = 'left'
        )
      )

    ## add trend line if activated
    if ( input[["trajectory_distribution_along_pseudotime_show_trend_line"]] == TRUE ) {

      ## calculate smoothened trend line
      trend_line = stats::ksmooth(
        cells_df[[ "pseudotime" ]],
        cells_df[[ color_variable ]],
        "normal",
        input[["trajectory_distribution_along_pseudotime_trend_line_bandwidth"]],
        x.points = cells_df[[ "pseudotime" ]]
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
          width = input[["trajectory_distribution_along_pseudotime_trend_line_width"]],
          color = "#e74c3c"
        ),
        name = "Trend line",
        hoverinfo = "text",
        text = ~glue::glue(
          "<b>Trend line</b>
          <b>Pseudotime:</b> {formatC(trend_line$x, format = 'f', big.mark = ',', digits = 3)}
          <b>{color_variable}:</b> {formatC(trend_line$y, format = 'f', big.mark = ',', digits = 3)}"
        ),
        showlegend = FALSE
      )
    }
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

observeEvent(input[["trajectory_distribution_along_pseudotime_info"]], {
  showModal(
    modalDialog(
      trajectory_distribution_along_pseudotime_info[["text"]],
      title = trajectory_distribution_along_pseudotime_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_distribution_along_pseudotime_info <- list(
  title = "Distribution along pseudotime",
  text = HTML("
    This plot shows the distribution of the variable selected above to color cells by along pseudotime.<br>
    If this is a categorical variable, e.g. 'sample' or 'cluster', you will see a density plot. Subgroups of the selected categorical variable that contain only a single cell will not be shown as no density can be calculated for them.<br>
    In contrast, if you have selected a continuous variable, e.g. nUMI or nGene, cells will be colored by the state they belong to."
  )
)
