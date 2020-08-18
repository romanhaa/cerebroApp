##----------------------------------------------------------------------------##
## Tab: Overview
##
## Projection.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Layout of the UI elements.
##----------------------------------------------------------------------------##

output[["overview_projection_UI"]] <- renderUI({
  fluidRow(
    ## selections and parameters
    column(width = 3, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = "Input parameters",
        tagList(
          uiOutput("overview_projection_parameters_UI"),
          uiOutput("overview_projection_scales_UI")
        )
      )
    ),
    ## plot
    column(width = 9, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = tagList(
          boxTitle("Dimensional reduction"),
          actionButton(
            inputId = "overview_projection_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 5px"
          ),
          shinySaveButton(
            "overview_projection_export",
            label = "export to PDF",
            title = "Export dimensional reduction to PDF file.",
            filetype = "pdf",
            viewtype = "icon",
            class = "btn-xs"
          )
        ),
        tagList(
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              "overview_projection",
              width = "auto",
              height = "85vh"
            ),
            type = 8,
            hide.ui = FALSE
          ),
          tags$br(),
          htmlOutput("overview_number_of_selected_cells"),
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI elements to set parameters for the projection.
##----------------------------------------------------------------------------##
output[["overview_projection_parameters_UI"]] <- renderUI({
  tagList(
    selectInput(
      "overview_projection_to_display",
      label = "Projection",
      choices = availableProjections()
    ),
    selectInput(
      "overview_point_color",
      label = "Color cells by",
      choices = colnames(getMetaData())[! colnames(getMetaData()) %in% c("cell_barcode")]
    ),
    sliderInput(
      "overview_percentage_cells_to_show",
      label = "Show % of cells",
      min = scatter_plot_percentage_cells_to_show[["min"]],
      max = scatter_plot_percentage_cells_to_show[["max"]],
      step = scatter_plot_percentage_cells_to_show[["step"]],
      value = scatter_plot_percentage_cells_to_show[["default"]]
    ),
    sliderInput(
      "overview_point_size",
      label = "Point size",
      min = scatter_plot_point_size[["min"]],
      max = scatter_plot_point_size[["max"]],
      step = scatter_plot_point_size[["step"]],
      value = scatter_plot_point_size[["default"]]
    ),
    sliderInput(
      "overview_point_opacity",
      label = "Point opacity",
      min = scatter_plot_point_opacity[["min"]],
      max = scatter_plot_point_opacity[["max"]],
      step = scatter_plot_point_opacity[["step"]],
      value = scatter_plot_point_opacity[["default"]]
    )
  )
})

##----------------------------------------------------------------------------##
## UI elements to select X and Y limits in projection.
##----------------------------------------------------------------------------##
output[["overview_projection_scales_UI"]] <- renderUI({
  projection_to_display <- if (
    is.null(input[["overview_projection_to_display"]]) ||
    is.na(input[["overview_projection_to_display"]])
  ) {
    availableProjections()[1]
  } else {
    input[["overview_projection_to_display"]]
  }

  range_x_min <- getProjection(projection_to_display)[,1] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
  range_x_max <- getProjection(projection_to_display)[,1] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
  range_y_min <- getProjection(projection_to_display)[,2] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
  range_y_max <- getProjection(projection_to_display)[,2] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
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
## Plotly plot of the selected projection.
##----------------------------------------------------------------------------##

output[["overview_projection"]] <- plotly::renderPlotly({

  ## don't proceed without these inputs
  req(
    input[["overview_projection_to_display"]],
    input[["overview_percentage_cells_to_show"]],
    input[["overview_point_color"]],
    input[["overview_point_size"]],
    input[["overview_point_opacity"]],
    input[["overview_scale_x_manual_range"]],
    input[["overview_scale_y_manual_range"]]
  )

  projection_to_display <- input[["overview_projection_to_display"]]
  cells_to_display <- getCellIDs()

  ## randomly remove cells
  if ( input[["overview_percentage_cells_to_show"]] < 100 ) {
    number_of_cells_to_plot <- ceiling(
      input[["overview_percentage_cells_to_show"]] / 100 * length(cells_to_display)
    )
    cells_to_display <- cells_to_display[ sample(1:length(cells_to_display), number_of_cells_to_plot) ]
  }

  ## extract cells to plot
  to_plot <- cbind(
      getProjection(projection_to_display)[ cells_to_display , ],
      getMetaData()[ cells_to_display , ]
    ) %>% 
    as.data.frame()

  ## put rows in random order
  to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]

  ## define colors
  if ( input[["overview_point_color"]] %in% getGroups() ) {
    colors_this_plot <- reactive_colors()[[ input[["overview_point_color"]] ]]
  } else if ( input[["overview_point_color"]] %in% getCellCycle() ) {
    colors_this_plot <- reactive_colors()[[ input[["overview_point_color"]] ]]
  } else if ( is.factor(to_plot[[ input[["overview_point_color"]] ]]) ) {
    colors_this_plot <- setNames(
      default_colorset[1:length(levels(to_plot[[ input[["overview_point_color"]] ]]))],
      levels(to_plot[[ input[["overview_point_color"]] ]])
    )
  } else if ( is.character(to_plot[[ input[["overview_point_color"]] ]]) ) {
    colors_this_plot <- setNames(
      default_colorset[1:length(unique(to_plot[[ input[["overview_point_color"]] ]]))],
      unique(to_plot[[ input[["overview_point_color"]] ]])
    )
  } else {
    NULL
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

  ## check if projection consists of 3 or 2 dimensions
  ## ... selected projection contains 3 dimensions
  if ( ncol(getProjection(projection_to_display)) == 3 ) {

    ## check if selected coloring variable is categorical or numeric
    ## ... selected coloring variable is numeric
    if ( is.numeric(to_plot[[ input[["overview_point_color"]] ]]) ) {
      plot <- plotly::plot_ly(
          to_plot,
          x = ~to_plot[,1],
          y = ~to_plot[,2],
          z = ~to_plot[,3],
          type = "scatter3d",
          mode = "markers",
          marker = list(
            colorbar = list(
              title = input[["overview_point_color"]]
            ),
            color = ~to_plot[[ input[["overview_point_color"]] ]],
            opacity = input[["overview_point_opacity"]],
            colorscale = "YlGnBu",
            reversescale = TRUE,
            line = list(
              color = "rgb(196,196,196)",
              width = 1
            ),
            size = input[["overview_point_size"]]
          ),
          hoverinfo = "text",
          text = ~tooltip_info,
          source = "overview_projection"
        )

    ## ... selected coloring variable is not numeric
    } else {
      plot <- plotly::plot_ly(
          to_plot,
          x = ~to_plot[,1],
          y = ~to_plot[,2],
          z = ~to_plot[,3],
          color = ~to_plot[[ input[["overview_point_color"]] ]],
          colors = colors_this_plot,
          type = "scatter3d",
          mode = "markers",
          marker = list(
            opacity = input[["overview_point_opacity"]],
            line = list(
              color = "rgb(196,196,196)",
              width = 1
            ),
            size = input[["overview_point_size"]]
          ),
          hoverinfo = "text",
          text = ~tooltip_info,
          source = "overview_projection"
        )
    }

    ## add layout to plot
    plot <- plot %>%
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

  ## ... selection projection consists of 2 dimensions
  } else if ( ncol(getProjection(projection_to_display)) == 2 ) {

    ## check if selected coloring variable is categorical or numeric
    ## ... selected coloring variable is numeric
    if ( is.numeric(to_plot[[ input[["overview_point_color"]] ]]) ) {
      plot <- plotly::plot_ly(
        to_plot,
        x = ~to_plot[,1],
        y = ~to_plot[,2],
        type = "scatter",
        mode = "markers",
        marker = list(
          colorbar = list(
            title = input[["overview_point_color"]]
          ),
          color = ~to_plot[[ input[["overview_point_color"]] ]],
          opacity = input[["overview_point_opacity"]],
          colorscale = "YlGnBu",
          reversescale = TRUE,
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["overview_point_size"]]
        ),
        hoverinfo = "text",
        text = ~tooltip_info,
        source = "overview_projection"
      )

    ## ... selected coloring variable is not numeric
    } else {
      plot <- plotly::plot_ly(
        to_plot,
        x = ~to_plot[,1],
        y = ~to_plot[,2],
        color = ~to_plot[[ input[["overview_point_color"]] ]],
        colors = colors_this_plot,
        type = "scatter",
        mode = "markers",
        marker = list(
          opacity = input[["overview_point_opacity"]],
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["overview_point_size"]]
        ),
        hoverinfo = "text",
        text = ~tooltip_info,
        source = "overview_projection"
      )
    }

    ## add layout to plot
    plot <- plot %>%
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

    ## return plot either with WebGL or without, depending on setting
    if ( preferences[["use_webgl"]] == TRUE ) {
      plot %>% plotly::toWebGL()
    } else {
      plot
    }
  }
})

##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##

output[["overview_number_of_selected_cells"]] <- renderText({

  ## don't proceed without these inputs
  req(
    input[["overview_projection_to_display"]]
  )

  ## check selection
  ## ... selection has not been made or there is not cell in it
  if (
    is.null(plotly::event_data("plotly_selected", source = "overview_projection")) ||
    length(plotly::event_data("plotly_selected", source = "overview_projection")) == 0
  ) {

    ## manually set counter to 0
    number_of_selected_cells <- 0

  ## ... selection has been made and at least 1 cell is in it
  } else {

    ## get number of selected cells
    number_of_selected_cells <- formatC(nrow(plotly::event_data("plotly_selected", source = "overview_projection")), format = "f", big.mark = ",", digits = 0)
  }

  ## prepare string to show
  paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
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
## Text in info box.
##----------------------------------------------------------------------------##

overview_projection_info <- list(
  title = "Dimensional reduction",
  text = p(
    "Interactive projection of cells into 2-dimensional space based on their expression profile.",
    tags$ul(
      tags$li("Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44)."),
      tags$li("Cells can be colored by the sample they came from, the cluster they were assigned, the number of transcripts or expressed genes, percentage of mitochondrial and ribosomal gene expression, an apoptotic score (calculated based on the expression of few marker genes; more info in the 'Sample info' tab on the left), or cell cycle status (determined using the Seurat and Cyclone method)."),
      tags$li("Confidence ellipses show the 95% confidence regions."),
      tags$li("Samples and clusters can be removed from the plot individually to highlight a contrast of interest."),
      tags$li("By default, the point size is set to 15 without any transparency but both these attributes can be changed using the sliders on the left. The point size can also be set to reflect the number of transcripts or expressed genes."),
      tags$li("The last 2 slider elements on the left can be used to resize the projection axes. This can be particularly useful when a projection contains a population of cell that is very far away from the rest and therefore creates a big empty space (which is not uncommon for UMAPs).")
    ),
    "The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow."
  )
)

##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##

observeEvent(input[["overview_projection_export"]], {

  ## don't proceed without these inputs
  req(
    input[["overview_projection_to_display"]],
    input[["overview_percentage_cells_to_show"]],
    input[["overview_point_color"]],
    input[["overview_point_size"]],
    input[["overview_point_opacity"]],
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

    ## get selected projection
    projection_to_display <- input[["overview_projection_to_display"]]
    cells_to_display <- getCellIDs()

    ## merge cell positions in projection and meta data
    to_plot <- cbind(
        getProjection(projection_to_display)[ cells_to_display , ],
        getMetaData()[ cells_to_display , ]
      ) %>% 
      as.data.frame()

    ## put rows in random order
    to_plot <- to_plot[ sample(1:nrow(to_plot)) , ]

    ## get X and Y scale limits
    xlim <- c(
      input[["overview_scale_x_manual_range"]][1],
      input[["overview_scale_x_manual_range"]][2]
    )
    ylim <- c(
      input[["overview_scale_y_manual_range"]][1],
      input[["overview_scale_y_manual_range"]][2]
    )

    ## check if selection projection consists of 2 or 3 dimensions
    ## ... selection projection consists of 3 dimensions
    if ( ncol(getProjection(projection_to_display)) == 3 ) {

      ## give error message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Sorry!",
        text = "It's currently not possible to create PDF plots from 3D dimensional reductions. Please use the PNG export button in the panel or a 2D dimensional reduction instead.",
        type = "error"
      )

    ## ... selection projection consists of 2 dimensions
    } else if ( ncol(getProjection(projection_to_display)) == 2 ) {

      ## check type of coloring variable
      ## ... type is of character or factor
      if (
        is.factor(to_plot[[ input[["overview_point_color"]] ]]) ||
        is.character(to_plot[[ input[["overview_point_color"]] ]])
      ) {

        ## check for known groups to retrieve assigned colors
        ## ... coloring variable is one of the grouping variables
        if ( input[["overview_point_color"]] %in% getGroups() ) {

          ## retrieve colors
          colors_this_plot <- reactive_colors()[[ input[["overview_point_color"]] ]]

        ## ... coloring variable is one of the cell cycle variables
        } else if ( input[["overview_point_color"]] %in% getCellCycle() ) {

          ## retrieve colors
          colors_this_plot <- reactive_colors()[[ input[["overview_point_color"]] ]]

        ## ... coloring variable is type "factor"
        } else if ( is.factor(to_plot[[ input[["overview_point_color"]] ]]) ) {

          ## assign default colors
          colors_this_plot <- setNames(
            default_colorset[1:length(levels(to_plot[[ input[["overview_point_color"]] ]]))],
            levels(to_plot[[ input[["overview_point_color"]] ]])
          )

        ## ... coloring variable is of type "character"
        } else if ( is.character(to_plot[[ input[["overview_point_color"]] ]]) ) {

          ## assign default colors
          colors_this_plot <- setNames(
            default_colorset[1:length(unique(to_plot[[ input[["overview_point_color"]] ]]))],
            unique(to_plot[[ input[["overview_point_color"]] ]])
          )

        ## ... coloring variable is none of the above
        } else {

          ## use default colors
          colors_this_plot <- default_colorset
        }

        ## prepare plot
        p <- ggplot(
            to_plot,
            aes_q(
              x = as.name(colnames(to_plot)[1]),
              y = as.name(colnames(to_plot)[2]),
              fill = as.name(input[["overview_point_color"]])
            )
          ) +
          geom_point(
            shape = 21,
            size = input[["overview_point_size"]]/3,
            stroke = 0.2,
            color = "#c4c4c4",
            alpha = input[["overview_point_opacity"]]
          ) +
          scale_fill_manual(values = colors_this_plot) +
          lims(x = xlim, y = ylim) +
          theme_bw()

      ## ... type is neither character nor factor, most likely numeric
      } else {

        ## prepare plot
        p <- ggplot(
            to_plot,
            aes_q(
              x = as.name(colnames(to_plot)[1]),
              y = as.name(colnames(to_plot)[2]),
              fill = as.name(input[["overview_point_color"]])
            )
          ) +
          geom_point(
            shape = 21,
            size = input[["overview_point_size"]]/3,
            stroke = 0.2,
            color = "#c4c4c4",
            alpha = input[["overview_point_opacity"]]
          ) +
          scale_fill_distiller(
            palette = "YlGnBu",
            direction = 1,
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
          ) +
          lims(x = xlim, y = ylim) +
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
  }
})
