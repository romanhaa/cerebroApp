##----------------------------------------------------------------------------##
## Tab: Overview
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI elements.
##----------------------------------------------------------------------------##
output[["overview_UI"]] <- renderUI({
  tagList(
    selectInput(
      "overview_projection_to_display",
      label = "Projection",
      choices = reducedDimNames(sample_data()$expression)
    ),
    ## allow selection of groups based on what group was used to color the cells?
    # shinyWidgets::pickerInput(
    #   "overview_samples_to_display",
    #   label = "Groups to display",
    #   choices = sample_data()$sample_names,
    #   selected = sample_data()$sample_names,
    #   options = list("actions-box" = TRUE),
    #   multiple = TRUE
    # ),
    sliderInput(
      "overview_percentage_cells_to_show",
      label = "Show % of cells",
      min = scatter_plot_percentage_cells_to_show[["min"]],
      max = scatter_plot_percentage_cells_to_show[["max"]],
      step = scatter_plot_percentage_cells_to_show[["step"]],
      value = scatter_plot_percentage_cells_to_show[["default"]]
    ),
    selectInput(
      "overview_dot_color",
      label = "Color cells by",
      choices = colnames(colData(sample_data()$expression))[! colnames(colData(sample_data()$expression)) %in% c("cell_barcode")]
    ),
    sliderInput(
      "overview_dot_size",
      label = "Dot size",
      min = scatter_plot_dot_size[["min"]],
      max = scatter_plot_dot_size[["max"]],
      step = scatter_plot_dot_size[["step"]],
      value = scatter_plot_dot_size[["default"]]
    ),
    sliderInput(
      "overview_dot_opacity",
      label = "Dot opacity",
      min = scatter_plot_dot_opacity[["min"]],
      max = scatter_plot_dot_opacity[["max"]],
      step = scatter_plot_dot_opacity[["step"]],
      value = scatter_plot_dot_opacity[["default"]]
    )
  )
})

##----------------------------------------------------------------------------##
## UI elements for X and Y limits in projection.
##----------------------------------------------------------------------------##
output[["overview_scales"]] <- renderUI({
  projection_to_display <- if ( is.null(input[["overview_projection_to_display"]]) || is.na(input[["overview_projection_to_display"]]) ) {
    reducedDimNames(sample_data()$expression)[1]
  } else {
    input[["overview_projection_to_display"]]
  }

  range_x_min <- reducedDim(sample_data()$expression, projection_to_display)[,1] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
  range_x_max <- reducedDim(sample_data()$expression, projection_to_display)[,1] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
  range_y_min <- reducedDim(sample_data()$expression, projection_to_display)[,2] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
  range_y_max <- reducedDim(sample_data()$expression, projection_to_display)[,2] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
  tagList(
    sliderInput(
      "overview_scale_x_manual_range",
      label = "X axis",
      min = range_x_min,
      max = range_x_max,
      value = c(range_x_min, range_x_max)
    ),
    sliderInput(
      "overview_scale_y_manual_range",
      label = "Y axis",
      min = range_y_min,
      max = range_y_max,
      value = c(range_y_min, range_y_max)
    )
  )
})

##----------------------------------------------------------------------------##
## Projection.
## TODO: tooltip should show all grouping variables
##----------------------------------------------------------------------------##

output[["overview_projection"]] <- plotly::renderPlotly({
  req(
    input[["overview_projection_to_display"]],
    # input[["overview_samples_to_display"]],
    # input[["overview_clusters_to_display"]],
    input[["overview_percentage_cells_to_show"]],
    input[["overview_dot_color"]],
    input[["overview_dot_size"]],
    input[["overview_dot_opacity"]],
    input[["overview_scale_x_manual_range"]],
    input[["overview_scale_y_manual_range"]]
  )

  projection_to_display <- input[["overview_projection_to_display"]]
  ## TODO: adapt if necessary
  # samples_to_display <- input[["overview_samples_to_display"]]
  # clusters_to_display <- input[["overview_clusters_to_display"]]
  # cells_to_display <- which(
  #     (sample_data()$cells$sample %in% samples_to_display) &
  #     (sample_data()$cells$cluster %in% clusters_to_display)
  #   )
  cells_to_display <- rownames(colData(sample_data()$expression))

  ## randomly remove cells
  if ( input[["overview_percentage_cells_to_show"]] < 100 ) {
    number_of_cells_to_plot <- ceiling(
      input[["overview_percentage_cells_to_show"]] / 100 * length(cells_to_display)
    )
    cells_to_display <- cells_to_display[ sample(1:length(cells_to_display), number_of_cells_to_plot) ]
  }

  ## extract cells to plot
  to_plot <- cbind(
      reducedDim(sample_data()$expression, projection_to_display)[ cells_to_display , ],
      colData(sample_data()$expression)[ cells_to_display , ]
    ) %>% 
    as.data.frame()
  to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]

  ## define colors
  if ( input[["overview_dot_color"]] %in% sample_data()$getGroups() ) {
    colors_this_plot <- reactive_colors()[[ input[["overview_dot_color"]] ]]
  } else if ( input[["overview_dot_color"]] %in% sample_data()$cell_cycle ) {
    colors_this_plot <- reactive_colors()[[ input[["overview_dot_color"]] ]]
  } else if ( is.factor(to_plot[[ input[["overview_dot_color"]] ]]) ) {
    colors_this_plot <- setNames(
      default_colorset[1:length(levels(to_plot[[ input[["overview_dot_color"]] ]]))],
      levels(to_plot[[ input[["overview_dot_color"]] ]])
    )
  } else if ( is.character(to_plot[[ input[["overview_dot_color"]] ]]) ) {
    colors_this_plot <- setNames(
      default_colorset[1:length(unique(to_plot[[ input[["overview_dot_color"]] ]]))],
      unique(to_plot[[ input[["overview_dot_color"]] ]])
    )
  } else {
    NULL
  }

  tooltip_info <- paste0(
    "<b>Cell</b>: ", to_plot[[ "cell_barcode" ]], "<br>",
    "<b>Transcripts</b>: ", formatC(to_plot[[ "nUMI" ]], format = "f", big.mark = ",", digits = 0), "<br>",
    "<b>Expressed genes</b>: ", formatC(to_plot[[ "nGene" ]], format = "f", big.mark = ",", digits = 0), "<br>"
  )

  for ( group in sample_data()$getGroups() ) {
    tooltip_info <- paste0(
      tooltip_info,
      "<b>", group, "</b>: ", to_plot[[ group ]], "<br>"
    )
  }

  if ( ncol(reducedDim(sample_data()$expression, projection_to_display)) == 3 ) {
    if ( is.numeric(to_plot[[ input[["overview_dot_color"]] ]]) ) {
      plotly::plot_ly(
        to_plot,
        x = ~to_plot[,1],
        y = ~to_plot[,2],
        z = ~to_plot[,3],
        type = "scatter3d",
        mode = "markers",
        marker = list(
          colorbar = list(
            title = input[["overview_dot_color"]]
          ),
          color = ~to_plot[[ input[["overview_dot_color"]] ]],
          opacity = input[["overview_dot_opacity"]],
          colorscale = "YlGnBu",
          reversescale = TRUE,
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["overview_dot_size"]]
        ),
        hoverinfo = "text",
        text = ~tooltip_info,
        source = "overview_projection"
      ) %>%
      plotly::layout(
        scene = list(
          xaxis = list(
            title = colnames(to_plot)[1],
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = colnames(to_plot)[2],
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          ),
          zaxis = list(
            title = colnames(to_plot)[3],
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          )
        ),
        hoverlabel = list(
          font = list(
            size = 11
          )
        )
      )
    } else {
      plotly::plot_ly(
        to_plot,
        x = ~to_plot[,1],
        y = ~to_plot[,2],
        z = ~to_plot[,3],
        color = ~to_plot[[ input[["overview_dot_color"]] ]],
        colors = colors_this_plot,
        type = "scatter3d",
        mode = "markers",
        marker = list(
          opacity = input[["overview_dot_opacity"]],
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["overview_dot_size"]]
        ),
        hoverinfo = "text",
        text = ~tooltip_info,
        source = "overview_projection"
      ) %>%
      plotly::layout(
        scene = list(
          xaxis = list(
            title = colnames(to_plot)[1],
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = colnames(to_plot)[2],
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          ),
          zaxis = list(
            title = colnames(to_plot)[3],
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          )
        ),
        hoverlabel = list(font = list(size = 11))
      )
    }
  } else {
    if ( is.numeric(to_plot[[ input[["overview_dot_color"]] ]]) ) {
      plot <- plotly::plot_ly(
        to_plot,
        x = ~to_plot[,1],
        y = ~to_plot[,2],
        type = "scatter",
        mode = "markers",
        marker = list(
          colorbar = list(
            title = input[["overview_dot_color"]]
          ),
          color = ~to_plot[[ input[["overview_dot_color"]] ]],
          opacity = input[["overview_dot_opacity"]],
          colorscale = "YlGnBu",
          reversescale = TRUE,
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["overview_dot_size"]]
        ),
        hoverinfo = "text",
        text = ~tooltip_info,
        source = "overview_projection"
      ) %>%
      plotly::layout(
        xaxis = list(
          title = colnames(to_plot)[1],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = input[["overview_scale_x_manual_range"]]
        ),
        yaxis = list(
          title = colnames(to_plot)[2],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = input[["overview_scale_y_manual_range"]]
        ),
        hoverlabel = list(font = list(size = 11))
      )
      if ( preferences[["use_webgl"]] == TRUE ) {
        plot %>% plotly::toWebGL()
      } else {
        plot
      }
    } else {
      plot <- plotly::plot_ly(
        to_plot,
        x = ~to_plot[,1],
        y = ~to_plot[,2],
        color = ~to_plot[[ input[["overview_dot_color"]] ]],
        colors = colors_this_plot,
        type = "scatter",
        mode = "markers",
        marker = list(
          opacity = input[["overview_dot_opacity"]],
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["overview_dot_size"]]
        ),
        hoverinfo = "text",
        text = ~tooltip_info,
        source = "overview_projection"
      ) %>%
      plotly::layout(
        xaxis = list(
          title = colnames(to_plot)[1],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = input[["overview_scale_x_manual_range"]]
        ),
        yaxis = list(
          title = colnames(to_plot)[2],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = input[["overview_scale_y_manual_range"]]
        ),
        hoverlabel = list(font = list(size = 11))
      )
      if ( preferences[["use_webgl"]] == TRUE ) {
        plot %>% plotly::toWebGL()
      } else {
        plot
      }
    }
  }
})

##----------------------------------------------------------------------------##
## Info button.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_projection_info"]], {
  showModal(
    modalDialog(
      overview_projection_info[["text"]],
      title = overview_projection_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Number of selected cells.
##----------------------------------------------------------------------------##

output[["overview_number_of_selected_cells"]] <- renderText({
  req(
    input[["overview_projection_to_display"]]
  )
  if (
    is.null(plotly::event_data("plotly_selected", source = "overview_projection")) ||
    length(plotly::event_data("plotly_selected", source = "overview_projection")) == 0
  ) {
    number_of_selected_cells <- 0
  } else {
    number_of_selected_cells <- nrow(plotly::event_data("plotly_selected", source = "overview_projection"))
  }
  paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
})

##----------------------------------------------------------------------------##
## Export projection.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_projection_export"]], {
  req(
    input[["overview_projection_to_display"]],
    # input[["overview_samples_to_display"]],
    # input[["overview_clusters_to_display"]],
    input[["overview_percentage_cells_to_show"]],
    input[["overview_dot_color"]],
    input[["overview_dot_size"]],
    input[["overview_dot_opacity"]],
    input[["overview_scale_x_manual_range"]],
    input[["overview_scale_y_manual_range"]]
  )

  ## open dialog to select where plot should be saved and how the file should
  ## be named
  shinyFileSave(
    input,
    id = "overview_projection_export",
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

    projection_to_display <- input[["overview_projection_to_display"]]
    # samples_to_display <- input[["overview_samples_to_display"]]
    # clusters_to_display <- input[["overview_clusters_to_display"]]
    # cells_to_display <- which(
    #     (sample_data()$cells$sample %in% samples_to_display) &
    #     (sample_data()$cells$cluster %in% clusters_to_display)
    #   )
    cells_to_display <- rownames(colData(sample_data()$expression))

    to_plot <- cbind(
        reducedDim(sample_data()$expression, projection_to_display)[ cells_to_display , ],
        colData(sample_data()$expression)[ cells_to_display , ]
      ) %>% 
      as.data.frame()
    to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]
    xlim <- c(
      input[["overview_scale_x_manual_range"]][1],
      input[["overview_scale_x_manual_range"]][2]
    )
    ylim <- c(
      input[["overview_scale_y_manual_range"]][1],
      input[["overview_scale_y_manual_range"]][2]
    )

    if ( ncol(reducedDim(sample_data()$expression, projection_to_display)) == 3 ) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Sorry!",
        text = "It's currently not possible to create PDF plots from 3D dimensional reductions. Please use the PNG export button in the panel or a 2D dimensional reduction instead.",
        type = "error"
      )
    } else {
      if ( is.factor(to_plot[[ input[["overview_dot_color"]] ]]) || is.character(to_plot[[ input[["overview_dot_color"]] ]]) ) {
        ## define colors
        if ( input[["overview_dot_color"]] %in% sample_data()$getGroups() ) {
          colors_this_plot <- reactive_colors()[[ input[["overview_dot_color"]] ]]
        } else if ( input[["overview_dot_color"]] %in% sample_data()$cell_cycle ) {
          colors_this_plot <- reactive_colors()[[ input[["overview_dot_color"]] ]]
        } else if ( is.factor(to_plot[[ input[["overview_dot_color"]] ]]) ) {
          colors_this_plot <- setNames(
            default_colorset[1:length(levels(to_plot[[ input[["overview_dot_color"]] ]]))],
            levels(to_plot[[ input[["overview_dot_color"]] ]])
          )
        } else if ( is.character(to_plot[[ input[["overview_dot_color"]] ]]) ) {
          colors_this_plot <- setNames(
            default_colorset[1:length(unique(to_plot[[ input[["overview_dot_color"]] ]]))],
            unique(to_plot[[ input[["overview_dot_color"]] ]])
          )
        } else {
          colors_this_plot <- default_colorset
        }
        p <- ggplot(
            to_plot,
            aes_q(
              x = as.name(colnames(to_plot)[1]),
              y = as.name(colnames(to_plot)[2]),
              fill = as.name(input[["overview_dot_color"]])
            )
          ) +
          geom_point(
            shape = 21,
            size = input[["overview_dot_size"]]/3,
            stroke = 0.2,
            color = "#c4c4c4",
            alpha = input[["overview_dot_opacity"]]
          ) +
          scale_fill_manual(values = colors_this_plot) +
          lims(x = xlim, y = ylim) +
          theme_bw()
      } else {
        p <- ggplot(
            to_plot,
            aes_q(
              x = as.name(colnames(to_plot)[1]),
              y = as.name(colnames(to_plot)[2]),
              fill = as.name(input[["overview_dot_color"]])
            )
          ) +
          geom_point(
            shape = 21,
            size = input[["overview_dot_size"]]/3,
            stroke = 0.2,
            color = "#c4c4c4",
            alpha = input[["overview_dot_opacity"]]
          ) +
          scale_fill_distiller(
            palette = "YlGnBu",
            direction = 1,
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
          ) +
          lims(x = xlim, y = ylim) +
          theme_bw()
      }

      # out_filename <- paste0(
      #     plot_export_path, "Cerebro_",
      #     gsub(
      #       sample_data()$getExperiment()$experiment_name,
      #       pattern = " ", replacement = "_"
      #     ),
      #     "_overview_", input[["overview_projection_to_display"]], "_by_",
      #     gsub(
      #       input[["overview_dot_color"]],
      #       pattern = "\\.", replacement = "_"
      #     ),
      #     ".pdf"
      #   )

      pdf(NULL)
      ggsave(file_output, p, height = 8, width = 11)

      if ( file.exists(file_output) ) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Success!",
          text = paste0("Plot saved successfully as: ", file_output),
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
    }
  }
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
    input[["overview_dot_color"]]
  )

  ## extract cells to plot
  to_plot <- cbind(
      reducedDim(sample_data()$expression, input[["overview_projection_to_display"]]),
      colData(sample_data()$expression)
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

  color_variable <- input[["overview_dot_color"]]

  ## if the selected coloring variable is categorical, represent the selected
  ## cells in a bar chart
  if (
    is.factor(to_plot[[ color_variable ]]) ||
    is.character(to_plot[[ color_variable ]])
  ) {
    ## calculate number of cells in each group
    t <- to_plot %>%
      dplyr::filter(group == 'selected')

    ##
    if ( nrow(t) > 0 ) {
      t <- t %>%
        dplyr::select(!!! rlang::syms(color_variable)) %>%
        dplyr::group_by_at(1) %>%
        dplyr::tally() %>%
        dplyr::ungroup()
    } else {
      t <- data.frame(
          group = sample_data()$getGroupLevels(input[["overview_dot_color"]]),
          n = 0
        ) %>%
        dplyr::rename(!!input[["overview_dot_color"]] := group)
    }

    ## convert factor to character to avoid empty bars when selecting cells of
    ## certain groups
    t[[1]] <- as.character(t[[1]])

    ## create color assignment for groups
    if ( input[["overview_dot_color"]] %in% sample_data()$getGroups() ) {
      colors_this_plot <- reactive_colors()[[ input[["overview_dot_color"]] ]]
    } else if ( input[["overview_dot_color"]] %in% sample_data()$cell_cycle ) {
      colors_this_plot <- reactive_colors()[[ input[["overview_dot_color"]] ]]
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

# info box
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
## Table for details of selected cells.
##----------------------------------------------------------------------------##

output[["overview_details_selected_cells_table"]] <- DT::renderDataTable(server = FALSE, {
  req(
    input[["overview_projection_to_display"]]
  )

  ## extract cells to plot
  to_plot <- cbind(
      reducedDim(sample_data()$expression, input[["overview_projection_to_display"]]),
      colData(sample_data()$expression)
    ) %>% 
    as.data.frame()
  ## if no selection is made or no cells are in selection, create empty table
  if (
    is.null(plotly::event_data("plotly_selected", source = "overview_projection")) ||
    length(plotly::event_data("plotly_selected", source = "overview_projection")) == 0
  ) {
    colData(sample_data()$expression) %>%
    as.data.frame() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()
  ## if at least 1 cell has been selected, extract meta data for selected cells
  } else {
    selected_cells <- plotly::event_data("plotly_selected", source = "overview_projection") %>%
      dplyr::mutate(identifier = paste0(x, '-', y))
    table <- to_plot %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(identifier = paste0(X1, '-', X2)) %>%
      dplyr::filter(identifier %in% selected_cells$identifier) %>%
      dplyr::select(-c(X1,X2,identifier)) %>%
      dplyr::select(cell_barcode, everything())
    ## if no cells end up in the table, e.g. because of a problem in cell
    ## filtering, create empty table
    if ( nrow(table) == 0 ) {
      colData(sample_data()$expression) %>%
      as.data.frame() %>%
      dplyr::slice(0) %>%
      prepareEmptyTable()
    ## if at least 1 cell remained in the table, as it should, create a proper
    ## table
    } else {
      prettifyTable(
        table,
        filter = list(position = "top", clear = TRUE),
        dom = "Brtlip",
        show_buttons = TRUE,
        number_formatting = input[["overview_details_selected_cells_table_number_formatting"]],
        color_highlighting = input[["overview_details_selected_cells_table_color_highlighting"]],
        hide_long_columns = TRUE,
        download_file_name = "overview_details_of_selected_cells"
      )
    }
  }
})

## info box
observeEvent(input[["overview_details_selected_cells_table_info"]], {
  showModal(
    modalDialog(
      overview_details_selected_cells_table_info$text,
      title = overview_details_selected_cells_table_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})
