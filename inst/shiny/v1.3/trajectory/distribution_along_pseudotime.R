##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Distribution along pseudotime.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_distribution_along_pseudotime_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Distribution along pseudotime"),
        cerebroInfoButton("trajectory_density_info")
      ),
      plotly::plotlyOutput("trajectory_density_plot")
    )
  )
})

##----------------------------------------------------------------------------##
## Plot.
##----------------------------------------------------------------------------##

output[["trajectory_density_plot"]] <- plotly::renderPlotly({

  ## don't do anything before these inputs are selected
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]],
    input[["trajectory_point_color"]]
  )

  ## collect trajectory data
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
  
  ## extract cells to plot
  to_plot <- cbind(
      trajectory_data[["meta"]],
      getMetaData()
    ) %>%
    dplyr::filter(!is.na(pseudotime))

  ## put rows in random order
  to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]

  ## grab column name for cell coloring
  color_variable <- input[["trajectory_point_color"]]

  ## ... cells are colored by a categorical variable; the Y axis will show the
  ##     density of the group along pseudotime
  if (
    is.factor(to_plot[[ color_variable ]]) ||
    is.character(to_plot[[ color_variable ]])
  ) {

    ## set colors for groups
    ## ... coloring variable is one of the grouping variables
    if ( input[["trajectory_point_color"]] %in% getGroups() ) {
      colors_this_plot <- reactive_colors()[[ input[["trajectory_point_color"]] ]]
    ## ... coloring variable is one of the cell cycle assignments
    } else if ( input[["trajectory_point_color"]] %in% getCellCycle() ) {
      colors_this_plot <- reactive_colors()[[ input[["trajectory_point_color"]] ]]
    ## ... coloring variable contains factors
    } else if ( is.factor(to_plot[[ input[["trajectory_point_color"]] ]]) ) {
      colors_this_plot <- setNames(
        default_colorset[1:length(levels(to_plot[[ input[["trajectory_point_color"]] ]]))],
        levels(to_plot[[ input[["trajectory_point_color"]] ]])
      )
    ## ... coloring variable containts characters
    } else if ( is.character(to_plot[[ input[["trajectory_point_color"]] ]]) ) {
      colors_this_plot <- setNames(
        default_colorset[1:length(unique(to_plot[[ input[["trajectory_point_color"]] ]]))],
        unique(to_plot[[ input[["trajectory_point_color"]] ]])
      )
    ## ... none of the above
    } else {
      colors_this_plot <- default_colorset
    }

    ## get group levels
    if ( is.factor(to_plot[[ color_variable ]]) ) {
      group_levels <- levels(to_plot[[ color_variable ]])
    } else if ( is.character(to_plot[[ color_variable ]]) ) {
      group_levels <- unique(to_plot[[ color_variable ]])
    }

    ## create empty plot
    plot <- plotly::plot_ly()

    ## add trace to plot for every group level
    for ( i in 1:length(group_levels) ) {

      ## get name of current group level
      current_group <- group_levels[i]

      ## filter cells for those that are in current group
      temp_data <- to_plot[which(to_plot[[ color_variable ]] == current_group),]

      ## calculate density over pseudotime
      temp_density <- stats::density(temp_data[["pseudotime"]], kernel = "gaussian")

      ## add alpha value to hex colors
      temp_color <- grDevices::col2rgb(colors_this_plot[i])
      temp_color <- grDevices::rgb(
        red = temp_color[1],
        green = temp_color[2],
        blue = temp_color[3],
        alpha = 175,
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
            color = colors_this_plot[i]
          ),
          hoverinfo = 'text',
          text = paste0(
            "<b>", current_group, "</b><br>",
            "<b>Pseudotime</b>: ", formatC(temp_density$x, format = "f", big.mark = ",", digits = 2), "<br>",
            "<b>Density</b>: ", formatC(temp_density$y, format = "f", big.mark = ",", digits = 2), "<br>"
          )
        )
    }

    ## add layout to plot
    plot %>%
    plotly::layout(
      xaxis = list(
        title = "Pseudotime",
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = range(to_plot[["pseudotime"]])
      ),
      yaxis = list(
        title = "Density",
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE
      )
    )

  ## ... cells should be colored by a numeric variable; instead of showing the
  ##     density by pseudotime, the numeric variable will be shown directly on
  ##     the Y axis
  } else {

    ## set colors for states
    colors_this_plot <- setNames(
      default_colorset[1:length(levels(trajectory_data[["meta"]]$state))],
      levels(trajectory_data[["meta"]]$state)
    )

    ## prepare tooltip/hover info
    tooltip_info <- paste0(
      "<b>Cell</b>: ", to_plot[[ "cell_barcode" ]], "<br>",
      "<b>Transcripts</b>: ", formatC(to_plot[[ "nUMI" ]], format = "f", big.mark = ",", digits = 0), "<br>",
      "<b>Expressed genes</b>: ", formatC(to_plot[[ "nGene" ]], format = "f", big.mark = ",", digits = 0), "<br>",
      "<b>Pseudotime</b>: ", formatC(to_plot[[ "pseudotime" ]], format = "f", big.mark = ",", digits = 2), "<br>",
      "<b>State</b>: ", to_plot[[ "state" ]], "<br>"
    )

    ## add info for known grouping variables to tooltip/hover
    for ( group in getGroups() ) {
      tooltip_info <- paste0(
        tooltip_info,
        "<b>", group, "</b>: ", to_plot[[ group ]], "<br>"
      )
    }

    ## prepare plot
    plot <- plotly::plot_ly(
      data = to_plot,
      x = ~pseudotime,
      y = ~to_plot[[ color_variable ]],
      type = "scatter",
      mode = "markers",
      color = ~state,
      colors = colors_this_plot,
      marker = list(
        opacity = input[["trajectory_point_opacity"]],
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["trajectory_point_size"]]
      ),
      hoverinfo = "text",
      text = ~tooltip_info
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
      hoverlabel = list(font = list(size = 11))
    )

    ## if set in options, return plot with WebGL
    if ( preferences$use_webgl == TRUE ) {
      plotly::toWebGL(plot)
    } else {
      plot
    }
  }
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_density_info"]], {
  showModal(
    modalDialog(
      trajectory_density_info[["text"]],
      title = trajectory_density_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_density_info <- list(
  title = "Distribution along pseudotime",
  text = p("This plot shows the distribution of the variable selected above to color cells by along pseudotime. If this is a categorical variable, e.g. 'sample' or 'cluster', you will see a density plot. In contrast, if you have selected a continuous variable, e.g. nUMI or nGene, cells will be colored by the state they belong to.")
)
