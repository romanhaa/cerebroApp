##----------------------------------------------------------------------------##
## Tab: Gene expression
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI elements for projection.
##----------------------------------------------------------------------------##
output[["expression_UI"]] <- renderUI({
  tagList(
    selectizeInput(
      'expression_genes_input',
      label = 'Gene(s)',
      choices = rownames(sample_data()$getExpression()),
      options = list(create = TRUE), multiple = TRUE
    ),
    selectInput(
      "expression_projection_to_display",
      label = "Projection",
      choices = sample_data()$availableProjections()
    ),
    # shinyWidgets::pickerInput(
    #   "expression_samples_to_display",
    #   label = "Samples to display",
    #   choices = sample_data()$sample_names,
    #   selected = sample_data()$sample_names,
    #   options = list("actions-box" = TRUE),
    #   multiple = TRUE
    # ),
    # shinyWidgets::pickerInput(
    #   "expression_clusters_to_display",
    #   label = "Clusters to display",
    #   choices = sample_data()$cluster_names,
    #   selected = sample_data()$cluster_names,
    #   options = list("actions-box" = TRUE),
    #   multiple = TRUE
    # ),
    sliderInput(
      "expression_percentage_cells_to_show",
      label = "Show % of cells",
      min = scatter_plot_percentage_cells_to_show[["min"]],
      max = scatter_plot_percentage_cells_to_show[["max"]],
      step = scatter_plot_percentage_cells_to_show[["step"]],
      value = scatter_plot_percentage_cells_to_show[["default"]]
    ),
    selectInput(
      "expression_projection_plotting_order",
      label = "Plotting order",
      choices = c("Random", "Highest expression on top"),
      selected = "Random"
    ),
    sliderInput(
      "expression_projection_dot_size",
      label = "Point size",
      min = scatter_plot_dot_size[["min"]],
      max = scatter_plot_dot_size[["max"]],
      step = scatter_plot_dot_size[["step"]],
      value = scatter_plot_dot_size[["default"]]
    ),
    sliderInput(
      "expression_projection_dot_opacity",
      label = "Point opacity",
      min = scatter_plot_dot_opacity[["min"]],
      max = scatter_plot_dot_opacity[["max"]],
      step = scatter_plot_dot_opacity[["step"]],
      value = scatter_plot_dot_opacity[["default"]]
    ),
    selectInput(
      "expression_projection_color_scale",
      label = "Color scale",
      choices = c("YlGnBu", "YlOrRd","Blues","Greens","Reds","RdBu","viridis"),
      selected = "YlGnBu"
    )
  )
})

##----------------------------------------------------------------------------##
## UI element for color scale range in projection.
##----------------------------------------------------------------------------##
output[["expression_color_scale_range"]] <- renderUI({
  range <- range(gene_expression_plot_data()$level)
  if ( range[1] == 0 & range[2] == 0 ) {
    range[2] = 1
  } else {
    range[1] <- range[1] %>% round(digits = 2)
    range[2] <- range[2] %>% round(digits = 2)
  }
  tagList(
    sliderInput(
      "expression_projection_color_scale_range",
      label = "Range of color scale",
      min = range[1],
      max = range[2],
      value = c(range[1], range[2])
    )
  )
})

##----------------------------------------------------------------------------##
## UI element for X and Y scales in projection.
##----------------------------------------------------------------------------##
output[["expression_scales"]] <- renderUI({
  req(
    input[["expression_projection_to_display"]]
  )
  projection_to_display <- input[["expression_projection_to_display"]]
  range_x_min <- sample_data()$getProjection(projection_to_display)[,1] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
  range_x_max <- sample_data()$getProjection(projection_to_display)[,1] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
  range_y_min <- sample_data()$getProjection(projection_to_display)[,2] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
  range_y_max <- sample_data()$getProjection(projection_to_display)[,2] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
  tagList(
    sliderInput(
      "expression_projection_scale_x_manual_range",
      label = "Range of X axis",
      min = range_x_min,
      max = range_x_max,
      value = c(range_x_min, range_x_max)
    ),
    sliderInput(
      "expression_projection_scale_y_manual_range",
      label = "Range of Y axis",
      min = range_y_min,
      max = range_y_max,
      value = c(range_y_min, range_y_max)
    )
  )
})

##----------------------------------------------------------------------------##
## Reactive data: Genes from user.
##----------------------------------------------------------------------------##

# cannot use req() because it delays initialization and plot is updated only with button press so plot doesn't initialize at all
genesToPlot <- reactive({
  genesToPlot <- list()
  if ( is.null(input[["expression_genes_input"]]) ) {
    genesToPlot[["genes_to_display"]] <- character()
  } else {
    genesToPlot[["genes_to_display"]] <- input[["expression_genes_input"]] %>%
      strsplit(",| |;|\n") %>%
      unlist() %>%
      gsub(pattern = " ", replacement = "", fixed = TRUE) %>%
      unique() %>%
      .[. != ""]
  }
  genesToPlot[["genes_to_display_here"]] <- rownames(sample_data()$getExpression())[ match(tolower(genesToPlot[["genes_to_display"]]), tolower(rownames(sample_data()$getExpression()))) ]
  genesToPlot[["genes_to_display_present"]] <- na.omit(genesToPlot[["genes_to_display_here"]])
  genesToPlot[["genes_to_display_missing"]] <- genesToPlot[["genes_to_display"]][ which(is.na(genesToPlot[["genes_to_display_here"]])) ]
  return(genesToPlot)
})

# select genes to be displayed
output[["expression_genes_displayed"]] <- renderText({
  paste0(
    "<b>Showing expression for ",
    length(genesToPlot()[["genes_to_display_present"]]), " gene(s):</b><br>",
    paste0(genesToPlot()[["genes_to_display_present"]], collapse = ", "),
    "<br><b>",
    length(genesToPlot()[["genes_to_display_missing"]]),
    " gene(s) are not in data set: </b><br>",
    paste0(genesToPlot()[["genes_to_display_missing"]], collapse = ", ")
  )
})

# data to plot
gene_expression_plot_data <- reactive({
  req(
    input[["expression_projection_to_display"]],
    # input[["expression_samples_to_display"]],
    # input[["expression_clusters_to_display"]],
    input[["expression_percentage_cells_to_show"]],
    input[["expression_projection_plotting_order"]],
    genesToPlot()
  )
  projection_to_display <- input[["expression_projection_to_display"]]
  # samples_to_display <- input[["expression_samples_to_display"]]
  # clusters_to_display <- input[["expression_clusters_to_display"]]
  percentage_cells_show <- input[["expression_percentage_cells_to_show"]]
  plot_order <- input[["expression_projection_plotting_order"]]
  ## TODO: adapt if necessary
  # check which cells to display
  # cells_to_display <- which(
  #     (sample_data()$cells$sample %in% samples_to_display) &
  #     (sample_data()$cells$cluster %in% clusters_to_display)
  #   )
  cells_to_display <- rownames(sample_data()$getMetaData())

  # randomly remove cells
  if ( percentage_cells_show < 100 ) {
    number_of_cells_to_plot <- ceiling(
      percentage_cells_show / 100 * length(cells_to_display)
    )
    cells_to_display <- cells_to_display[ sample(1:length(cells_to_display), number_of_cells_to_plot) ]
  }
  plot <- cbind(
      sample_data()$getProjection(projection_to_display)[ cells_to_display , ],
      sample_data()$getMetaData()[ cells_to_display , ]
    )
  if ( length(genesToPlot()$genes_to_display_present) == 0 ) {
    plot$level <- 0
  } else if ( length(genesToPlot()$genes_to_display_present) == 1 ) {
    plot$level <- genesToPlot()$genes_to_display_present %>%
      sample_data()$getExpression()[ . , cells_to_display ]
  } else {
    plot$level <- genesToPlot()$genes_to_display_present %>%
      sample_data()$getExpression()[ . , cells_to_display ] %>%
      Matrix::colMeans()
  }
  if ( plot_order == "Random" ) {
    plot <- sample(1:nrow(plot), nrow(plot)) %>%
      plot[ . , ]
  } else if ( plot_order == "Highest expression on top" ) {
    plot <- plot[ order(plot$level, decreasing = FALSE) , ]
  }
  return(plot)
})

##----------------------------------------------------------------------------##
## Projection.
##----------------------------------------------------------------------------##

output[["expression_projection"]] <- plotly::renderPlotly({

  ## don't proceed without these inputs
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_dot_size"]],
    input[["expression_projection_dot_opacity"]],
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_scale_range"]],
    input[["expression_projection_scale_x_manual_range"]],
    input[["expression_projection_scale_y_manual_range"]]
  )

  ## check selected color scale
  ## ... selected color scale is "viridis"
  if ( input[["expression_projection_color_scale"]] == 'viridis' ) {
    color_scale <- 'Viridis'

  ## ... selected color scale is anything else than "viridis"
  } else {
    color_scale <- input[["expression_projection_color_scale"]]
  }

  ## prepare tooltip/hover info
  tooltip_info <- paste0(
    "<b>Cell</b>: ", gene_expression_plot_data()[[ "cell_barcode" ]], "<br>",
    "<b>Transcripts</b>: ", formatC(gene_expression_plot_data()[[ "nUMI" ]], format = "f", big.mark = ",", digits = 0), "<br>",
    "<b>Expressed genes</b>: ", formatC(gene_expression_plot_data()[[ "nGene" ]], format = "f", big.mark = ",", digits = 0), "<br>",
    "<b>Expression level</b>: ", formatC(gene_expression_plot_data()$level, format = "f", big.mark = ",", digits = 3), "<br>"
  )

  ## add info for known grouping variables to tooltip/hover
  for ( group in sample_data()$getGroups() ) {
    tooltip_info <- paste0(
      tooltip_info,
      "<b>", group, "</b>: ", gene_expression_plot_data()[[ group ]], "<br>"
    )
  }

  ## check if selection projection consists of 2 or 3 dimensions
  ## ... selection projection consists of 3 dimensions
  if ( ncol(sample_data()$getProjection(input[["expression_projection_to_display"]])) == 3 ) {

    ## prepare plot
    plotly::plot_ly(
      gene_expression_plot_data(),
      x = gene_expression_plot_data()[,1],
      y = gene_expression_plot_data()[,2],
      z = gene_expression_plot_data()[,3],
      type = "scatter3d",
      mode = "markers",
      marker = list(
        colorbar = list(
          title = "Expression"
        ),
        color = ~level,
        opacity = input[["expression_projection_dot_opacity"]],
        colorscale = color_scale,
        cauto = FALSE,
        cmin = input[["expression_projection_color_scale_range"]][1],
        cmax = input[["expression_projection_color_scale_range"]][2],
        reversescale = TRUE,
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["expression_projection_dot_size"]]
      ),
      hoverinfo = "text",
      text = ~tooltip_info,
      source = "expression_projection"
    ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(
          title = colnames(gene_expression_plot_data())[1],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = colnames(gene_expression_plot_data())[2],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        zaxis = list(
          title = colnames(gene_expression_plot_data())[3],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        )
      ),
      hoverlabel = list(
        font = list(
          size = 11,
          color = "black"
        ),
        bgcolor = "lightgrey"
      )
    )

  ## ... selection projection consists of 2 dimensions
  } else if ( ncol(sample_data()$getProjection(input[["expression_projection_to_display"]])) == 2 ) {

    ## prepare plot
    plot <- plotly::plot_ly(
        gene_expression_plot_data(),
        x = gene_expression_plot_data()[,1],
        y = gene_expression_plot_data()[,2],
        type = "scatter",
        mode = "markers",
        marker = list(
          colorbar = list(
            title = "Expression"
          ),
          color = ~level,
          opacity = input[["expression_projection_dot_opacity"]],
          colorscale = color_scale,
          cauto = FALSE,
          cmin = input[["expression_projection_color_scale_range"]][1],
          cmax = input[["expression_projection_color_scale_range"]][2],
          reversescale = TRUE,
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["expression_projection_dot_size"]]
        ),
        hoverinfo = "text",
        text = ~tooltip_info,
        source = "expression_projection"
      ) %>%
      plotly::layout(
        xaxis = list(
          title = colnames(gene_expression_plot_data())[1],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = c(
            input[["expression_projection_scale_x_manual_range"]][1],
            input[["expression_projection_scale_x_manual_range"]][2]
          )
        ),
        yaxis = list(
          title = colnames(gene_expression_plot_data())[2],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = c(
            input[["expression_projection_scale_y_manual_range"]][1],
            input[["expression_projection_scale_y_manual_range"]][2]
          )
        ),
        dragmode = "pan",
        hoverlabel = list(
          font = list(
            size = 11,
            color = "black"
          ),
          bgcolor = "lightgrey"
        )
      )

    ## return plot either with WebGL or without, depending on setting
    if ( preferences$use_webgl == TRUE ) {
      plot %>% plotly::toWebGL()
    } else {
      plot
    }
  }
})

##----------------------------------------------------------------------------##
## Info box.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_projection_info"]], {
  showModal(
    modalDialog(
      expression_projection_info$text,
      title = expression_projection_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Number of selected cells.
##----------------------------------------------------------------------------##

output[["expression_number_of_selected_cells"]] <- renderText({

  ## don't proceed without these inputs
  req(
    input[["expression_projection_to_display"]]
  )

  ## check selection
  ## ... selection has not been made or there is not cell in it
  if (
    is.null(plotly::event_data("plotly_selected", source = "expression_projection")) ||
    length(plotly::event_data("plotly_selected", source = "expression_projection")) == 0
  ) {

    ## manually set counter to 0
    number_of_selected_cells <- 0

  ## ... selection has been made and at least 1 cell is in it
  } else {

    ## get number of selected cells
    number_of_selected_cells <- nrow(plotly::event_data("plotly_selected", source = "expression_projection"))
  }

  ## prepare string to show
  paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
})

##----------------------------------------------------------------------------##
## Export function.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_projection_export"]], {

  ## don't proceed without these inputs
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_plotting_order"]],
    input[["expression_projection_dot_size"]],
    input[["expression_projection_dot_opacity"]],
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_scale_range"]],
    input[["expression_projection_scale_x_manual_range"]],
    input[["expression_projection_scale_y_manual_range"]]
  )

  ## open dialog to select where plot should be saved and how the file should
  ## be named
  shinyFileSave(
    input,
    id = "expression_projection_export",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  ## retrieve info from dialog
  fileinfo <- parseSavePath(volumes, input[["expression_projection_export"]])

  ## only proceed if a path has been provided
  if ( nrow(fileinfo) > 0 ) {

    ## extract specified file path
    file_output <- as.character(fileinfo$datapath[1])

    ## make ggplot2 functions available
    require("ggplot2")

    ## get X and Y scale limits
    xlim <- c(
      input[["expression_projection_scale_x_manual_range"]][1],
      input[["expression_projection_scale_x_manual_range"]][2]
    )
    ylim <- c(
      input[["expression_projection_scale_y_manual_range"]][1],
      input[["expression_projection_scale_y_manual_range"]][2]
    )

    # ## build file name based on how many genes were selected
    # if ( length(genesToPlot()$genes_to_display_present) == 0 ) {
    #   out_filename <- paste0(
    #     plot_export_path, "Cerebro_",
    #     sample_data()$experiment$experiment_name, "_gene_expression_none"
    #   )
    # } else if ( length(genesToPlot()$genes_to_display_present) == 1 ) {
    #   out_filename <- paste0(
    #     plot_export_path, "Cerebro_",
    #     sample_data()$experiment$experiment_name, "_gene_expression_",
    #     genesToPlot()$genes_to_display_present, "_",
    #     input[["expression_projection_to_display"]]
    #   )
    # } else {
    #   out_filename <- paste0(
    #     plot_export_path, "Cerebro_",
    #     sample_data()$experiment$experiment_name, "_gene_expression_",
    #     genesToPlot()$genes_to_display_present[1],
    #     "_and_others_", input[["expression_projection_to_display"]]
    #   )
    # }

    # ## add info to file name
    # if ( input[["expression_projection_plotting_order"]] == "Random" ) {
    #   out_filename <- paste0(out_filename, "_random_order.pdf")
    # } else if ( input[["expression_projection_plotting_order"]] == "Highest expression on top" ) {
    #   out_filename <- paste0(out_filename, "_highest_expression_on_top.pdf")
    # }

    ## check if selection projection consists of 2 or 3 dimensions
    ## ... selection projection consists of 3 dimensions
    if ( ncol(sample_data()$getProjection(input[["expression_projection_to_display"]])) == 3 ) {

      ## give error message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Sorry!",
        text = "It's currently not possible to create PDF plots from 3D dimensional reductions. Please use the PNG export button in the panel or a 2D dimensional reduction instead.",
        type = "error"
      )

    ## ... selection projection consists of 2 dimensions
    } else if ( ncol(sample_data()$getProjection(input[["expression_projection_to_display"]])) == 2 ) {

      ## prepare plot
      p <- ggplot(
          gene_expression_plot_data(),
          aes_q(
            x = as.name(colnames(gene_expression_plot_data())[1]),
            y = as.name(colnames(gene_expression_plot_data())[2]),
            fill = as.name("level")
          )
        ) +
        geom_point(
          shape = 21,
          size = input[["expression_projection_dot_size"]]/3,
          stroke = 0.2,
          color = "#c4c4c4",
          alpha = input[["expression_projection_dot_opacity"]]
        ) +
        lims(x = xlim, y = ylim) +
        theme_bw()

        ## check if selected color scale
        ## ... selected color scale is "viridis"
        if ( input[["expression_projection_color_scale"]] == 'viridis' ) {

          ## add color scale to plot
          p <- p + viridis::scale_fill_viridis(
            option = "viridis",
            limits = input[["expression_projection_color_scale_range"]],
            oob = scales::squish,
            direction = -1,
            name = "Log-normalised\nexpression",
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
          )

        ## ... selected color scale is anything else than "viridis"
        } else {

          ## add color scale to plot
          p <- p + scale_fill_distiller(
            palette = input[["expression_projection_color_scale"]],
            limits = input[["expression_projection_color_scale_range"]],
            oob = scales::squish,
            direction = 1,
            name = "Log-normalised\nexpression",
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
          )
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
  }
})

##----------------------------------------------------------------------------##
## Table for details of selected cells.
##----------------------------------------------------------------------------##

output[["expression_details_selected_cells"]] <- DT::renderDataTable(server = FALSE, {

  ## check selection
  ## ... selection has not been made or there is not cell in it
  if (
    is.null(plotly::event_data("plotly_selected", source = "expression_projection")) ||
    length(plotly::event_data("plotly_selected", source = "expression_projection")) == 0
  ) {

    ## prepare empty table
    sample_data()$getMetaData() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()

  ## ... selection has been made and at least 1 cell is in it
  } else {

    ## get info of selected cells and create identifier from X-Y coordinates
    selected_cells <- plotly::event_data("plotly_selected", source = "expression_projection") %>%
      dplyr::mutate(identifier = paste0(x, '-', y))

    ## filter out non-selected cells with X-Y identifier and select some meta
    ## data
    table <- gene_expression_plot_data() %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(identifier = paste0(X1, '-', X2)) %>%
      dplyr::filter(identifier %in% selected_cells$identifier) %>%
      dplyr::select(-c(X1, X2, identifier)) %>%
      dplyr::rename(expression_level = level) %>%
      dplyr::select(cell_barcode, expression_level, everything())

    ## check how many cells are left after filtering
    ## ... no cells are left
    if ( nrow(table) == 0 ) {

      ## prepare empty table
      sample_data()$getMetaData() %>%
      dplyr::slice(0) %>%
      prepareEmptyTable()

    ## ... at least 1 cell is left
    } else {

      ## prepare proper table
      prettifyTable(
        table,
        filter = list(position = "top", clear = TRUE),
        dom = "Brtlip",
        show_buttons = TRUE,
        number_formatting = input[["expression_details_selected_cells_number_formatting"]],
        color_highlighting = input[["expression_details_selected_cells_color_highlighting"]],
        hide_long_columns = TRUE,
        download_file_name = "expression_details_of_selected_cells"
      )
    }
  }
})

# info box
observeEvent(input[["expression_details_selected_cells_info"]], {
  showModal(
    modalDialog(
      expression_details_selected_cells_info$text,
      title = expression_details_selected_cells_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Expression in selected cells.
##----------------------------------------------------------------------------##

# violin + box plot
output[["expression_in_selected_cells"]] <- plotly::renderPlotly({
  if (
    is.null(plotly::event_data("plotly_selected", source = "expression_projection")) |
    length(plotly::event_data("plotly_selected", source = "expression_projection")) == 0
  ) {
    data <- gene_expression_plot_data() %>% dplyr::mutate(group = 'not selected')
  } else {
    selected_cells <- plotly::event_data("plotly_selected", source = "expression_projection") %>%
      dplyr::mutate(identifier = paste0(x, '-', y))
    data <- gene_expression_plot_data() %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(
        identifier = paste0(X1, '-', X2),
        group = ifelse(identifier %in% selected_cells$identifier, 'selected', 'not selected'),
        group = factor(group, levels = c('selected', 'not selected'))
      ) %>%
      dplyr::select(group, level)
  }
  plotly::plot_ly(
    data,
    x = ~group,
    y = ~level,
    type = "violin",
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    ),
    color = ~group,
    colors = setNames(c('#e74c3c','#7f8c8d'),c('selected', 'not selected')),
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
      title = "Expression level",
      range = c(0, max(data$level) * 1.2),
      hoverformat = ".2f",
      mirror = TRUE,
      showline = TRUE
    ),
    dragmode = "select",
    hovermode = "compare"
  )
})

# info box
observeEvent(input[["expression_in_selected_cells_info"]], {
  showModal(
    modalDialog(
      expression_in_selected_cells_info$text,
      title = expression_in_selected_cells_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Expression by group
##----------------------------------------------------------------------------##

## UI element to select grouping variable
output[["expression_by_group_selected_group_UI"]] <- renderUI({
  selectInput(
    "expression_by_group_selected_group",
    label = "Select a group to show expression by:",
    choices = sample_data()$getGroups(),
    width = "100%"
  )
})

# box plot
output[["expression_by_group"]] <- plotly::renderPlotly({

  ## don't proceed without these inputs
  req(
    input[["expression_by_group_selected_group"]]
  )

  ## prepare plot
  gene_expression_plot_data() %>%
  plotly::plot_ly(
    x = ~.[[ input[["expression_by_group_selected_group"]] ]],
    y = ~level,
    type = "violin",
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    ),
    color = ~.[[ input[["expression_by_group_selected_group"]] ]],
    colors = reactive_colors()[[ input[["expression_by_group_selected_group"]] ]],
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
      title = "Expression level",
      range = c(0, max(gene_expression_plot_data()$level) * 1.2),
      hoverformat = ".2f",
      mirror = TRUE,
      showline = TRUE
    ),
    dragmode = "select",
    hovermode = "compare"
  )
})

# info box
observeEvent(input[["expression_by_group_info"]], {
  showModal(
    modalDialog(
      expression_by_group_info$text,
      title = expression_by_group_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Expression by gene.
##----------------------------------------------------------------------------##

## bar plot
output[["expression_by_gene"]] <- plotly::renderPlotly({

  ## don't proceed without these inputs
  req(
    input[["expression_projection_color_scale"]]
  )

  ## 
  ## ...
  if ( length(genesToPlot()$genes_to_display_present) == 0 ) {

    ##
    expression_levels <- data.frame(
      "gene" = character(),
      "expression" = integer()
    )

  ## ...
  } else if ( length(genesToPlot()$genes_to_display_present) == 1 ) {

    ##
    expression_levels <- data.frame(
      "gene" = genesToPlot()$genes_to_display_present,
      "expression" = mean(sample_data()$getExpression()[ genesToPlot()$genes_to_display_present , ])
    )

  ## ...
  } else {

    ## 
    expression_levels <- data.frame(
      "gene" = rownames(sample_data()$getExpression()[ genesToPlot()$genes_to_display_present , ]),
      "expression" = Matrix::rowMeans(sample_data()$getExpression()[ genesToPlot()$genes_to_display_present , ])
    ) %>%
    arrange(-expression) %>%
    top_n(50, expression)
  }

  ## color scale
  ## ...
  if ( input[["expression_projection_color_scale"]] == 'viridis' ) {
    color_scale <- 'Viridis'

  ## ...
  } else {
    color_scale <- input[["expression_projection_color_scale"]]
  }

  ##
  plotly::plot_ly(
    expression_levels,
    x = ~gene,
    y = ~expression,
    text = ~paste0(
      expression_levels$gene, ': ',
      format(expression_levels$expression, digits = 3)
    ),
    type = "bar",
    marker = list(
      color = ~expression,
      colorscale = color_scale,
      reversescale = TRUE,
      line = list(
        color = "rgb(196,196,196)",
        width = 1
      )
    ),
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
  plotly::layout(
    title = "",
    xaxis = list(
      title = "",
      type = "category",
      categoryorder = "array",
      categoryarray = expression_levels$gene,
      mirror = TRUE,
      showline = TRUE
    ),
    yaxis = list(
      title = "Expression level",
      mirror = TRUE,
      showline = TRUE
    ),
    dragmode = "select",
    hovermode = "compare"
  )
})

## info box
observeEvent(input[["expression_by_gene_info"]], {
  showModal(
    modalDialog(
      expression_by_gene_info[["text"]],
      title = expression_by_gene_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})
