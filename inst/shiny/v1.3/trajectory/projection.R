##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Projection with trajectory.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI elements for plot of projection and input parameters.
##----------------------------------------------------------------------------##

output[["trajectory_projection_UI"]] <- renderUI({
  tagList(
    fluidRow(
      column(width = 3, offset = 0, style = "padding: 0px;",
        cerebroBox(
          title = "Input parameters",
          tagList(
            uiOutput("trajectory_projection_input")
          )
        )
      ),
      column(width = 9, offset = 0, style = "padding: 0px;",
        cerebroBox(
          title = tagList(
            boxTitle("Trajectory"),
            actionButton(
              inputId = "trajectory_projection_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-right: 5px"
            ),
            actionButton(
              inputId = "trajectory_projection_export",
              label = "export to PDF",
              icon = NULL,
              class = "btn-xs",
              title = "Export trajectory to PDF file."
            )
          ),
          tagList(
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(
                "trajectory_projection",
                width = "auto",
                height = "85vh"
              ),
              type = 8,
              hide.ui = FALSE
            ),
            tags$br(),
            htmlOutput("trajectory_number_of_selected_cells")
          )
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI elements for input parameters of projection plot.
##----------------------------------------------------------------------------##

output[["trajectory_projection_input"]] <- renderUI({
  tagList(
    selectInput(
      "trajectory_point_color",
      label = "Color cells by",
      choices = c(
        "state", "pseudotime",
        colnames(getMetaData())[! colnames(getMetaData()) %in% c("cell_barcode")]
      )
    ),
    sliderInput(
      "trajectory_percentage_cells_to_show",
      label = "Show % of cells",
      min = scatter_plot_percentage_cells_to_show[["min"]],
      max = scatter_plot_percentage_cells_to_show[["max"]],
      step = scatter_plot_percentage_cells_to_show[["step"]],
      value = scatter_plot_percentage_cells_to_show[["default"]]
    ),
    sliderInput(
      "trajectory_point_size",
      label = "Point size",
      min = scatter_plot_point_size[["min"]],
      max = scatter_plot_point_size[["max"]],
      step = scatter_plot_point_size[["step"]],
      value = scatter_plot_point_size[["default"]]
    ),
    sliderInput(
      "trajectory_point_opacity",
      label = "Point opacity",
      min = scatter_plot_point_opacity[["min"]],
      max = scatter_plot_point_opacity[["max"]],
      step = scatter_plot_point_opacity[["step"]],
      value = scatter_plot_point_opacity[["default"]]
    )
  )
})

##----------------------------------------------------------------------------##
## Plot of projection.
##----------------------------------------------------------------------------##

output[["trajectory_projection"]] <- plotly::renderPlotly({

  ## don't do anything before these inputs are selected
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]],
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_point_color"]],
    input[["trajectory_point_size"]],
    input[["trajectory_point_opacity"]]
  )

  ## collect trajectory data
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  cells_to_display <- getCellIDs()

  ## randomly remove cells
  if ( input[["trajectory_percentage_cells_to_show"]] < 100 ) {
    number_of_cells_to_plot <- ceiling(
      input[["trajectory_percentage_cells_to_show"]] / 100 * length(cells_to_display)
    )
    cells_to_display <- cells_to_display[ sample(1:length(cells_to_display), number_of_cells_to_plot) ]
  }

  ## extract cells to plot
  to_plot <- cbind(
      trajectory_data[["meta"]][ cells_to_display , ],
      getMetaData()[ cells_to_display , ]
    ) %>%
    dplyr::filter(!is.na(pseudotime))
  to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]

  ## convert edges of trajectory into list format to plot with plotly
  trajectory_edges <- trajectory_data[["edges"]]
  trajectory_lines <- list()
  for (i in 1:nrow(trajectory_edges) ) {
    line = list(
      type = "line",
      line = list(color = "black"),
      xref = "x",
      yref = "y",
      x0 = trajectory_edges$source_dim_1[i],
      y0 = trajectory_edges$source_dim_2[i],
      x1 = trajectory_edges$target_dim_1[i],
      y1 = trajectory_edges$target_dim_2[i]
    )
    trajectory_lines <- c(trajectory_lines, list(line))
  }

  ## prepare tooltip/hover info
  tooltip_info <- paste0(
    "<b>Cell</b>: ", to_plot[[ "cell_barcode" ]], "<br>",
    "<b>Transcripts</b>: ", formatC(to_plot[[ "nUMI" ]], format = "f", big.mark = ",", digits = 0), "<br>",
    "<b>Expressed genes</b>: ", formatC(to_plot[[ "nGene" ]], format = "f", big.mark = ",", digits = 0), "<br>"
  )

  ## add info for known grouping variables to tooltip/hover
  for ( group in getGroups() ) {
    tooltip_info <- paste0(
      tooltip_info,
      "<b>", group, "</b>: ", to_plot[[ group ]], "<br>"
    )
  }

  ##
  if (
    is.factor(to_plot[[ input[["trajectory_point_color"]] ]]) ||
    is.character(to_plot[[ input[["trajectory_point_color"]] ]])
  ) {

    ##
    if ( input[["trajectory_point_color"]] %in% getGroups() ) {
      colors_this_plot <- reactive_colors()[[ input[["trajectory_point_color"]] ]]
    } else if ( input[["trajectory_point_color"]] %in% getCellCycle() ) {
      colors_this_plot <- reactive_colors()[[ input[["trajectory_point_color"]] ]]
    } else if ( is.factor(to_plot[[ input[["trajectory_point_color"]] ]]) ) {
      colors_this_plot <- setNames(
        default_colorset[1:length(levels(to_plot[[ input[["trajectory_point_color"]] ]]))],
        levels(to_plot[[ input[["trajectory_point_color"]] ]])
      )
    } else if ( is.character(to_plot[[ input[["trajectory_point_color"]] ]]) ) {
      colors_this_plot <- setNames(
        default_colorset[1:length(unique(to_plot[[ input[["trajectory_point_color"]] ]]))],
        unique(to_plot[[ input[["trajectory_point_color"]] ]])
      )
    }

    ##
    plot <- plotly::plot_ly(
      to_plot,
      x = ~DR_1,
      y = ~DR_2,
      color = ~to_plot[[ input[["trajectory_point_color"]] ]],
      colors = colors_this_plot,
      type = "scatter",
      mode = "markers",
      marker = list(
        opacity = input[["trajectory_point_opacity"]],
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["trajectory_point_size"]]
      ),
      hoverinfo = "text",
      text = ~tooltip_info,
      source = "trajectory_projection"
    )

  ##
  } else {

    ##
    plot <- plotly::plot_ly(
      data = to_plot,
      x = ~DR_1,
      y = ~DR_2,
      type = "scatter",
      mode = "markers",
      marker = list(
        colorbar = list(
          title = colnames(to_plot)[which(colnames(to_plot) == input[["trajectory_point_color"]])]
        ),
        color = ~to_plot[[ input[["trajectory_point_color"]] ]],
        opacity = input[["trajectory_point_opacity"]],
        colorscale = "YlGnBu",
        reversescale = TRUE,
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["trajectory_point_size"]]
      ),
      hoverinfo = "text",
      text = ~tooltip_info,
      source = "trajectory_projection"
    )
  }

  ## add layout to plot
  plot <- plot %>%
    plotly::layout(
      shapes = trajectory_lines,
      xaxis = list(
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = range(to_plot$DR_1) * 1.1
      ),
      yaxis = list(
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = range(to_plot$DR_2) * 1.1
      ),
      hoverlabel = list(font = list(size = 11))
    )

  ## return plot either with WebGL or without, depending on setting
  if ( preferences$use_webgl == TRUE ) {
    plotly::toWebGL(plot)
  } else {
    plot
  }

})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_projection_info"]], {
  showModal(
    modalDialog(
      trajectory_projection_info[["text"]],
      title = trajectory_projection_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_info <- list(
  title = "Trajectory",
  text = p("This plot shows cells projected into trajectory space, colored by the specified meta info, e.g. sample or cluster. The path of the trajectory is shown as a black line. Specific to this analysis, every cell has a 'pseudotime' and a transcriptional 'state' which corresponds to its position along the trajectory path.")
)

##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##

output[["trajectory_number_of_selected_cells"]] <- renderText({

  ## don't proceed without these inputs
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  ## check selection
  ## ... selection has not been made or there is not cell in it
  if (
    is.null(plotly::event_data("plotly_selected", source = "trajectory_projection")) ||
    length(plotly::event_data("plotly_selected", source = "trajectory_projection")) == 0
  ) {

    ## manually set counter to 0
    number_of_selected_cells <- 0

  ## ... selection has been made and at least 1 cell is in it
  } else {

    ## get number of selected cells
    number_of_selected_cells <- formatC(nrow(plotly::event_data("plotly_selected", source = "trajectory_projection")), format = "f", big.mark = ",", digits = 0)
  }

  ## prepare string to show
  paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
})

##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_projection_export"]], {

  ##
  req(
    input[["trajectory_to_display"]],
    input[["trajectory_samples_to_display"]],
    input[["trajectory_clusters_to_display"]],
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_point_color"]],
    input[["trajectory_point_size"]],
    input[["trajectory_point_opacity"]]
  )

  ## open dialog to select where plot should be saved and how the file should
  ## be named
  shinyFileSave(
    input,
    id = "trajectory_projection_export",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  ## retrieve info from dialog
  fileinfo <- parseSavePath(volumes, input[["overview_projection_export"]])

  ## only proceed if a path has been provided
  if ( nrow(fileinfo) > 0 ) {

    ## extract specified file path
    file_output <- as.character(fileinfo$datapath[1])

    ## ggplot2 functions are necessary to create the plot
    require("ggplot2")

    trajectory_data <- getTrajectory(
      input[["trajectory_selected_method"]],
      input[["trajectory_selected_name"]]
    )

    cells_to_display <- getCellIDs()

    to_plot <- cbind(
        trajectory_data[[ "meta" ]][ cells_to_display , ],
        getMetaData()[ cells_to_display , ]
      ) %>%
      dplyr::filter(!is.na(pseudotime))
    to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]

    ##
    input[["trajectory_point_color"]] <- input[["trajectory_point_color"]]

    if (
      is.factor(to_plot[[ input[["trajectory_point_color"]] ]]) ||
      is.character(to_plot[[ input[["trajectory_point_color"]] ]])
    ) {

      ##
      if ( input[["trajectory_point_color"]] %in% getGroups() ) {
        colors_this_plot <- reactive_colors()[[ input[["trajectory_point_color"]] ]]
      } else if ( input[["trajectory_point_color"]] %in% getCellCycle() ) {
        colors_this_plot <- reactive_colors()[[ input[["trajectory_point_color"]] ]]
      } else if ( is.factor(to_plot[[ input[["trajectory_point_color"]] ]]) ) {
        colors_this_plot <- setNames(
          default_colorset[1:length(levels(to_plot[[ input[["trajectory_point_color"]] ]]))],
          levels(to_plot[[ input[["trajectory_point_color"]] ]])
        )
      } else if ( is.character(to_plot[[ input[["trajectory_point_color"]] ]]) ) {
        colors_this_plot <- setNames(
          default_colorset[1:length(unique(to_plot[[ input[["trajectory_point_color"]] ]]))],
          unique(to_plot[[ input[["trajectory_point_color"]] ]])
        )
      } else {
        colors_this_plot <- default_colorset
      }

      p <- ggplot() +
        geom_point(
          data = to_plot,
          aes_string(x = colnames(to_plot)[1], y = colnames(to_plot)[2], fill = input[["trajectory_point_color"]]),
          shape = 21,
          size = input[["trajectory_point_size"]]/3,
          stroke = 0.2,
          color = "#c4c4c4",
          alpha = input[["trajectory_point_opacity"]]
        ) +
        geom_segment(
          data = trajectory_data[["edges"]],
          aes(source_dim_1, source_dim_2, xend = target_dim_1, yend = target_dim_2),
          size = 0.75, linetype = "solid", na.rm = TRUE
        ) +
        scale_fill_manual(values = colors_this_plot) +
        theme_bw()
    } else {
      p <- ggplot() +
        geom_point(
          data = to_plot,
          aes_string(x = colnames(to_plot)[1], y = colnames(to_plot)[2], fill = input[["trajectory_point_color"]]),
          shape = 21,
          size = input[["trajectory_point_size"]]/3,
          stroke = 0.2,
          color = "#c4c4c4",
          alpha = input[["trajectory_point_opacity"]]
        ) +
        geom_segment(
          data = trajectory_data[["edges"]],
          aes(source_dim_1, source_dim_2, xend = target_dim_1, yend = target_dim_2),
          size = 0.75, linetype = "solid", na.rm = TRUE
        ) +
        scale_fill_distiller(
          palette = "YlGnBu",
          direction = 1,
          guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
        ) +
        theme_bw()
    }

    ## save plot
    pdf(NULL)
    ggsave(file_output, p, height = 8, width = 11)

    ## check if file was succesfully saved
    ## ... successful
    if ( file.exists(file_output) ) {

      ## give positive message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Success!",
        text = paste0("Plot saved successfully as: ", file_output),
        type = "success"
      )

    ## ... failed
    } else {

      ## give negative message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error!",
        text = "Sorry, it seems something went wrong...",
        type = "error"
      )
    }
  }
})
