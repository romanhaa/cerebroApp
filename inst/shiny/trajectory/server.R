##----------------------------------------------------------------------------##
## Tab: Trajectory.
##----------------------------------------------------------------------------##

# what needs to be done
# - check if trajectory data is available
# - if not, display a message
# - if yes, merge with rest of meta data (so cells can be colored by whatever)
#   - maybe just merge with those cells that info is available for? not ideal
# - plot trajectory with ggplot -> plotly
# - second plot to show selected meta data over pseudotime
# - let user show gene expression as well? probably more complicated


# UI element: display results or alternative text
output[["trajectory_UI"]] <- renderUI({
  if ( length(sample_data()$trajectory) > 0 ) {
    tagList(
      fluidRow(
        column(width = 3, offset = 0, style = "padding: 0px;",
          cerebroBox(
            title = "Input parameters",
            tagList(
              uiOutput("trajectory_input")
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
            plotly::plotlyOutput(
              "trajectory_projection",
              width = "auto",
              height = "85vh"
            )
          )
        )
      ),
      cerebroBox(
        title = "Distribution along pseudotime",
        tagList(
#          shiny::plotOutput("trajectory_density"),
          plotly::plotlyOutput("trajectory_density_plotly")
        )
      )
    )
  } else {
    cerebroBox(
      title = "Trajectory",
      tagList(
        textOutput("trajectory_missing")
      )
    )
  }
})

# alternative text
output[["trajectory_missing"]] <- renderText({
  "No trajectories available to display."
})

##----------------------------------------------------------------------------##
## UI elements.
##----------------------------------------------------------------------------##
output[["trajectory_input"]] <- renderUI({
  tagList(
    selectInput(
      "trajectory_to_display",
      label = "Trajectory",
      choices = names(sample_data()$trajectory)
    ),
    shinyWidgets::pickerInput(
      "trajectory_samples_to_display",
      label = "Samples to display",
      choices = sample_data()$sample_names,
      selected = sample_data()$sample_names,
      options = list("actions-box" = TRUE),
      multiple = TRUE
    ),
    shinyWidgets::pickerInput(
      "trajectory_clusters_to_display",
      label = "Clusters to display",
      choices = sample_data()$cluster_names,
      selected = sample_data()$cluster_names,
      options = list("actions-box" = TRUE),
      multiple = TRUE
    ),
    sliderInput(
      "trajectory_percentage_cells_to_show",
      label = "Show % of cells",
      min = scatter_plot_percentage_cells_to_show[["min"]],
      max = scatter_plot_percentage_cells_to_show[["max"]],
      step = scatter_plot_percentage_cells_to_show[["step"]],
      value = scatter_plot_percentage_cells_to_show[["default"]]
    ),
    selectInput(
      "trajectory_dot_color",
      label = "Color cells by",
      choices = c("state","pseudotime",names(sample_data()$cells)[! names(sample_data()$cells) %in% c("cell_barcode")])
    ),
    sliderInput(
      "trajectory_dot_size",
      label = "Dot size",
      min = scatter_plot_dot_size[["min"]],
      max = scatter_plot_dot_size[["max"]],
      step = scatter_plot_dot_size[["step"]],
      value = scatter_plot_dot_size[["default"]]
    ),
    sliderInput(
      "trajectory_dot_opacity",
      label = "Dot opacity",
      min = scatter_plot_dot_opacity[["min"]],
      max = scatter_plot_dot_opacity[["max"]],
      step = scatter_plot_dot_opacity[["step"]],
      value = scatter_plot_dot_opacity[["default"]]
    )
  )
})

##----------------------------------------------------------------------------##
## Projection.
##----------------------------------------------------------------------------##
output[["trajectory_projection"]] <- plotly::renderPlotly({
  # don't do anything before these inputs are selected
  req(
    input[["trajectory_to_display"]],
    input[["trajectory_samples_to_display"]],
    input[["trajectory_clusters_to_display"]],
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_dot_color"]],
    input[["trajectory_dot_size"]],
    input[["trajectory_dot_opacity"]]
  )

  trajectory_to_display <- input[["trajectory_to_display"]]
  samples_to_display <- input[["trajectory_samples_to_display"]]
  clusters_to_display <- input[["trajectory_clusters_to_display"]]
  cells_to_display <- which(
      grepl(
        sample_data()$cells$sample,
        pattern = paste0("^", samples_to_display, "$", collapse="|")
      ) &
      grepl(
        sample_data()$cells$cluster,
        pattern = paste0("^", clusters_to_display, "$", collapse="|")
      )
    )

  # randomly remove cells
  if ( input[["trajectory_percentage_cells_to_show"]] < 100 ) {
    number_of_cells_to_plot <- ceiling(
      input[["trajectory_percentage_cells_to_show"]] / 100 * length(cells_to_display)
    )
    cells_to_display <- cells_to_display[ sample(1:length(cells_to_display), number_of_cells_to_plot) ]
  }

  # extract cells to plot
  to_plot <- base::cbind(
      sample_data()$trajectory[[ trajectory_to_display ]][["meta"]][ cells_to_display , ],
      sample_data()$cells[ cells_to_display , ]
    )
  to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]

  color_variable <- input[["trajectory_dot_color"]]

  # convert edges of trajectory into list format to plot with plotly
  trajectory_edges <- sample_data()$trajectory[[trajectory_to_display]][["edges"]]
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

  if ( is.factor(to_plot[[ color_variable ]]) || is.character(to_plot[[ color_variable ]]) ) {
    cols <- if ( color_variable == "sample" ) {
      sample_data()$samples$colors
    } else if ( color_variable == "cluster" ) {
      sample_data()$clusters$colors
    } else if ( color_variable %in% c("cell_cycle_seurat","cell_cycle_cyclone") ) {
      cell_cycle_colorset
    } else if ( is.factor(to_plot[[ color_variable ]]) ) {
      setNames(colors[1:length(levels(to_plot[[ color_variable ]]))], levels(to_plot[[ color_variable ]]))
    } else {
      colors
    }
    plot <- plotly::plot_ly(
      to_plot,
      x = ~DR_1,
      y = ~DR_2,
      color = ~to_plot[[ color_variable ]],
      colors = cols,
      type = "scatter",
      mode = "markers",
      marker = list(
        opacity = input[["trajectory_dot_opacity"]],
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["trajectory_dot_size"]]
      ),
      hoverinfo = "text",
      text = ~paste(
        "<b>Cell</b>: ", to_plot[ , "cell_barcode" ], "<br>",
        "<b>Sample</b>: ", to_plot[ , "sample" ], "<br>",
        "<b>Cluster</b>: ", to_plot[ , "cluster" ], "<br>",
        "<b>Transcripts</b>: ", formatC(to_plot[ , "nUMI" ], format = "f", big.mark = ",", digits = 0), "<br>",
        "<b>Expressed genes</b>: ", formatC(to_plot[ , "nGene" ], format = "f", big.mark = ",", digits = 0), "<br>",
        "<b>State</b>: ", to_plot[ , "state" ], "<br>",
        "<b>Pseudotime</b>: ", round(to_plot[ , "pseudotime" ], 3)
      )
    ) %>%
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
    if ( preferences$use_webgl == TRUE ) {
      plot %>% plotly::toWebGL()
    } else {
      plot
    }
  } else {
    plot <- plotly::plot_ly(
      data = to_plot,
      x = ~DR_1,
      y = ~DR_2,
      type = "scatter",
      mode = "markers",
      marker = list(
        colorbar = list(
          title = colnames(to_plot)[which(colnames(to_plot) == color_variable)]
        ),
        color = ~to_plot[[ color_variable ]],
        opacity = input[["trajectory_dot_opacity"]],
        colorscale = "YlGnBu",
        reversescale = TRUE,
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["trajectory_dot_size"]]
      ),
      hoverinfo = "text",
      text = ~paste(
        "<b>Cell</b>: ", to_plot[ , "cell_barcode" ], "<br>",
        "<b>Sample</b>: ", to_plot[ , "sample" ], "<br>",
        "<b>Cluster</b>: ", to_plot[ , "cluster" ], "<br>",
        "<b>Transcripts</b>: ", formatC(to_plot[ , "nUMI" ], format = "f", big.mark = ",", digits = 0), "<br>",
        "<b>Expressed genes</b>: ", formatC(to_plot[ , "nGene" ], format = "f", big.mark = ",", digits = 0), "<br>",
        "<b>State</b>: ", to_plot[ , "state" ], "<br>",
        "<b>Pseudotime</b>: ", round(to_plot[ , "pseudotime" ], 3)
      )
    ) %>%
    plotly::layout(
      shapes = trajectory_lines,
      xaxis = list(
        title = colnames(to_plot)[1],
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = range(to_plot$DR_1) * 1.1
      ),
      yaxis = list(
        title = colnames(to_plot)[2],
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = range(to_plot$DR_2) * 1.1
      ),
      hoverlabel = list(font = list(size = 11))
    )
    if ( preferences$use_webgl == TRUE ) {
      plotly::toWebGL(plot)
    } else {
      plot
    }
  }
})

##----------------------------------------------------------------------------##
## Info button.
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
## Distribution along pseudotime.
##----------------------------------------------------------------------------##
# output[["trajectory_density"]] <- shiny::renderPlot({
#   # don't do anything before these inputs are selected
#   req(
#     input[["trajectory_to_display"]],
#     input[["trajectory_samples_to_display"]],
#     input[["trajectory_clusters_to_display"]],
#     input[["trajectory_dot_color"]]
#   )

#   trajectory_to_display <- input[["trajectory_to_display"]]
#   samples_to_display <- input[["trajectory_samples_to_display"]]
#   clusters_to_display <- input[["trajectory_clusters_to_display"]]
#   cells_to_display <- which(
#       grepl(
#         sample_data()$cells$sample,
#         pattern = paste0("^", samples_to_display, "$", collapse="|")
#       ) &
#       grepl(
#         sample_data()$cells$cluster,
#         pattern = paste0("^", clusters_to_display, "$", collapse="|")
#       )
#     )

#   # extract cells to plot
#   to_plot <- base::cbind(
#       sample_data()$trajectory[[ trajectory_to_display ]][["meta"]][ cells_to_display , ],
#       sample_data()$cells[ cells_to_display , ]
#     )
#   to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]

#   color_variable <- input[["trajectory_dot_color"]]

#   colors <- if ( color_variable == "sample" ) {
#     sample_data()$samples$colors
#   } else if ( color_variable == "cluster" ) {
#     sample_data()$clusters$colors
#   } else if ( color_variable == "state" ) {
#     sample_data()$clusters$colors
#   } else if ( color_variable %in% c("cell_cycle_seurat","cell_cycle_cyclone") ) {
#     cell_cycle_colorset
#   } else if ( is.factor(to_plot[[ color_variable ]]) ) {
#     setNames(colors[1:length(levels(to_plot[[ color_variable ]]))], levels(to_plot[[ color_variable ]]))
#   } else {
#     colors
#   }

#   ggplot(to_plot, aes_string(x = "pseudotime", fill = color_variable)) +
#     geom_density(alpha = 0.4) +
#     theme_bw() +
#     labs(x = "Pseudotime", y = "Density") +
#     scale_fill_manual(values = colors) +
#     guides(fill = guide_legend(override.aes = list(alpha = 1)))
# })

output[["trajectory_density_plotly"]] <- plotly::renderPlotly({
  # don't do anything before these inputs are selected
  req(
    input[["trajectory_to_display"]],
    input[["trajectory_samples_to_display"]],
    input[["trajectory_clusters_to_display"]],
    input[["trajectory_dot_color"]]
  )

  trajectory_to_display <- input[["trajectory_to_display"]]
  samples_to_display <- input[["trajectory_samples_to_display"]]
  clusters_to_display <- input[["trajectory_clusters_to_display"]]
  cells_to_display <- which(
      grepl(
        sample_data()$cells$sample,
        pattern = paste0("^", samples_to_display, "$", collapse="|")
      ) &
      grepl(
        sample_data()$cells$cluster,
        pattern = paste0("^", clusters_to_display, "$", collapse="|")
      )
    )

  # extract cells to plot
  to_plot <- base::cbind(
      sample_data()$trajectory[[ trajectory_to_display ]][["meta"]][ cells_to_display , ],
      sample_data()$cells[ cells_to_display , ]
    )
  to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]

  color_variable <- input[["trajectory_dot_color"]]

  if ( is.factor(to_plot[[ color_variable ]]) || is.character(to_plot[[ color_variable ]]) ) {
    cols <- if ( color_variable == "sample" ) {
      sample_data()$samples$colors
    } else if ( color_variable == "cluster" ) {
      sample_data()$clusters$colors
    } else if ( color_variable %in% c("cell_cycle_seurat","cell_cycle_cyclone") ) {
      cell_cycle_colorset
    } else if ( is.factor(to_plot[[ color_variable ]]) ) {
      setNames(colors[1:length(levels(to_plot[[ color_variable ]]))], levels(to_plot[[ color_variable ]]))
    } else {
      colors
    }
    p <- ggplot(to_plot, aes_string(x = "pseudotime", fill = color_variable)) +
      geom_density(alpha = 0.4, color = "black") +
      theme_bw() +
      labs(x = "Pseudotime", y = "Density") +
      scale_fill_manual(values = cols) +
      guides(fill = guide_legend(override.aes = list(alpha = 1)))
    plotly::ggplotly(p, tooltip = "text") %>%
    plotly::style(
      hoveron = "fill"
    )
  } else {
    plot <- plotly::plot_ly(
      data = to_plot,
      x = ~pseudotime,
      y = ~to_plot[[ color_variable ]],
      type = "scatter",
      mode = "markers",
      color = ~state,
      colors = colors,
      marker = list(
        opacity = input[["trajectory_dot_opacity"]],
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["trajectory_dot_size"]]
      ),
      hoverinfo = "text",
      text = ~paste(
        "<b>Cell</b>: ", to_plot[ , "cell_barcode" ], "<br>",
        "<b>Sample</b>: ", to_plot[ , "sample" ], "<br>",
        "<b>Cluster</b>: ", to_plot[ , "cluster" ], "<br>",
        "<b>Transcripts</b>: ", formatC(to_plot[ , "nUMI" ], format = "f", big.mark = ",", digits = 0), "<br>",
        "<b>Expressed genes</b>: ", formatC(to_plot[ , "nGene" ], format = "f", big.mark = ",", digits = 0), "<br>",
        "<b>State</b>: ", to_plot[ , "state" ], "<br>",
        "<b>Pseudotime</b>: ", round(to_plot[ , "pseudotime" ], 3)
      )
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
    if ( preferences$use_webgl == TRUE ) {
      plotly::toWebGL(plot)
    } else {
      plot
    }
  }
})

##----------------------------------------------------------------------------##
## Export projection.
##----------------------------------------------------------------------------##
observeEvent(input[["trajectory_projection_export"]], {
  req(
    input[["trajectory_to_display"]],
    input[["trajectory_samples_to_display"]],
    input[["trajectory_clusters_to_display"]],
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_dot_color"]],
    input[["trajectory_dot_size"]],
    input[["trajectory_dot_opacity"]]
  )
  library("ggplot2")
  if ( exists("plot_export_path") ) {
    trajectory_to_display <- input[["trajectory_to_display"]]
    samples_to_display <- input[["trajectory_samples_to_display"]]
    clusters_to_display <- input[["trajectory_clusters_to_display"]]
    cells_to_display <- which(
      grepl(
        sample_data()$cells$sample,
        pattern = paste0("^", samples_to_display, "$", collapse = "|")
      ) &
      grepl(
        sample_data()$cells$cluster,
        pattern = paste0("^", clusters_to_display, "$", collapse = "|")
      )
    )
    to_plot <- cbind(
      sample_data()$trajectory[[ trajectory_to_display ]][[ "meta" ]][ cells_to_display , ],
      sample_data()$cells[ cells_to_display , ]
    )
    to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]

    color_variable <- input[["trajectory_dot_color"]]

    if ( is.factor(to_plot[[ color_variable ]]) || is.character(to_plot[[ color_variable ]]) ) {
      if ( color_variable == "sample" ) {
        cols <- sample_data()$samples$colors
      } else if ( color_variable == "cluster" ) {
        cols <- sample_data()$clusters$colors
      } else if ( color_variable %in% c("cell_cycle_seurat","cell_cycle_cyclone") ) {
        cols <- cell_cycle_colorset
      } else if ( is.factor(to_plot[ , color_variable ]) ) {
        cols <- setNames(colors[1:length(levels(to_plot[ , color_variable ]))], levels(to_plot[ , color_variable ]))
      } else {
        cols <- colors
      }
      p <- ggplot() +
        geom_point(
          data = to_plot,
          aes_string(x = colnames(to_plot)[1], y = colnames(to_plot)[2], fill = color_variable),
          shape = 21,
          size = input[["trajectory_dot_size"]]/3,
          stroke = 0.2,
          color = "#c4c4c4",
          alpha = input[["trajectory_dot_opacity"]]
        ) +
        geom_segment(
          data = sample_data()$trajectory[[ trajectory_to_display ]]$edges,
          aes(source_dim_1, source_dim_2, xend = target_dim_1, yend = target_dim_2),
          size = 0.75, linetype = "solid", na.rm = TRUE
        ) +
        scale_fill_manual(values = cols) +
        theme_bw()
    } else {
      p <- ggplot() +
        geom_point(
          data = to_plot,
          aes_string(x = colnames(to_plot)[1], y = colnames(to_plot)[2], fill = color_variable),
          shape = 21,
          size = input[["trajectory_dot_size"]]/3,
          stroke = 0.2,
          color = "#c4c4c4",
          alpha = input[["trajectory_dot_opacity"]]
        ) +
        geom_segment(
          data = sample_data()$trajectory[[ trajectory_to_display ]]$edges,
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

    out_filename <- paste0(
        plot_export_path, "Cerebro_",
        gsub(
          sample_data()$experiment$experiment_name,
          pattern = " ", replacement = "_"
        ),
        "_trajectory_", trajectory_to_display, "_by_",
        gsub(
          color_variable,
          pattern = "\\.", replacement = "_"
        ),
        ".pdf"
      )

    pdf(NULL)
    ggsave(out_filename, p, height = 8, width = 11)

    if ( file.exists(out_filename) ) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Success!",
        text = paste0("Plot saved successfully as: ", out_filename),
        type = "success"
      )
    } else {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error!",
        text = "Sorry, it seems something went wrong...",
        type = "error"
      )
    }
  } else {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error!",
      text = "Sorry, we couldn't find a place to store the figure. Please submit an issue on GitHub @ https://github.com/romanhaa/cerebroApp",
      type = "error"
    )
  }
})