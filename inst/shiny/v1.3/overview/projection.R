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
        title = tagList(
          "Main parameters",
          actionButton(
            inputId = "overview_projection_main_parameters_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-left: 5px"
          )
        ),
        uiOutput("overview_projection_main_parameters_UI")
      ),
      cerebroBox(
        title = tagList(
          "Additional parameters",
          actionButton(
            inputId = "overview_projection_additional_parameters_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-left: 5px"
          )
        ),
        uiOutput("overview_projection_additional_parameters_UI"),
        collapsed = TRUE
      ),
      cerebroBox(
        title = tagList(
          "Group filters",
          actionButton(
            inputId = "overview_projection_group_filters_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-left: 5px"
          )
        ),
        uiOutput("overview_projection_group_filters_UI"),
        collapsed = TRUE
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
            title = "Show additional information for this panel.",
            icon = NULL,
            class = "btn-xs",
            style = "margin-right: 3px"
          ),
          shinyFiles::shinySaveButton(
            "overview_projection_export",
            label = "export to PDF",
            title = "Export dimensional reduction to PDF file.",
            filetype = "pdf",
            viewtype = "icon",
            class = "btn-xs",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("overview_projection_scales_UI")
            ),
            circle = FALSE,
            icon = icon("cog"),
            inline = TRUE,
            size = "xs"
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
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##

output[["overview_projection_main_parameters_UI"]] <- renderUI({

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
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["overview_projection_main_parameters_info"]], {
  showModal(
    modalDialog(
      overview_projection_main_parameters_info[["text"]],
      title = overview_projection_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

overview_projection_main_parameters_info <- list(
  title = "Main parameters for projection",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li>
      <li><b>Color cells by:</b> Select which variable, categorical or continuous, from the meta data should be used to color the cells.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements to set additional parameters for the projection.
##----------------------------------------------------------------------------##

output[["overview_projection_additional_parameters_UI"]] <- renderUI({
  tagList(
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
    ),
    sliderInput(
      "overview_percentage_cells_to_show",
      label = "Show % of cells",
      min = scatter_plot_percentage_cells_to_show[["min"]],
      max = scatter_plot_percentage_cells_to_show[["max"]],
      step = scatter_plot_percentage_cells_to_show[["step"]],
      value = scatter_plot_percentage_cells_to_show[["default"]]
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_additional_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["overview_projection_additional_parameters_info"]], {
  showModal(
    modalDialog(
      overview_projection_additional_parameters_info[["text"]],
      title = overview_projection_additional_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
# <li><b>Range of X/Y axis (located in dropdown menu above the projection):</b> Set the X/Y axis limits. This is useful when you want to change the aspect ratio of the plot.</li>

overview_projection_additional_parameters_info <- list(
  title = "Additional parameters for projection",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements to set group filters for the projection.
##----------------------------------------------------------------------------##

output[["overview_projection_group_filters_UI"]] <- renderUI({
  group_filters <- list()
  for ( i in getGroups() ) {
    group_filters[[i]] <- shinyWidgets::pickerInput(
      paste0("overview_projection_group_filter_", i),
      label = i,
      choices = getGroupLevels(i),
      selected = getGroupLevels(i),
      options = list("actions-box" = TRUE),
      multiple = TRUE
    )
  }
  group_filters
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["overview_projection_group_filters_info"]], {
  showModal(
    modalDialog(
      overview_projection_group_filters_info[["text"]],
      title = overview_projection_group_filters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

overview_projection_group_filters_info <- list(
  title = "Group filters for projection",
  text = HTML("
    The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that are pass all filters (for each grouping variable) are shown in the projection.
    "
  )
)

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_group_filters_UI",
  suspendWhenHidden = FALSE
)

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

  XYranges <- getXYranges(getProjection(projection_to_display))

  tagList(
    sliderInput(
      "overview_scale_x_manual_range",
      label = "Range of X axis",
      min = XYranges$x$min,
      max = XYranges$x$max,
      value = c(XYranges$x$min, XYranges$x$max)
    ),
    sliderInput(
      "overview_scale_y_manual_range",
      label = "Range of Y axis",
      min = XYranges$y$min,
      max = XYranges$y$max,
      value = c(XYranges$y$min, XYranges$y$max)
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_scales_UI",
  suspendWhenHidden = FALSE
)

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

  ## build data frame with data
  cells_df <- cbind(getProjection(projection_to_display), getMetaData())

  ## available group filters
  group_filters <- names(input)[grepl(names(input), pattern = 'overview_projection_group_filter_')]

  ## remove cells based on group filters
  for ( i in group_filters ) {
    group <- strsplit(i, split = 'overview_projection_group_filter_')[[1]][2]
    if ( group %in% colnames(cells_df) ) {
      cells_df <- cells_df[which(cells_df[[group]] %in% input[[i]] ),]
    }
  }

  ## randomly remove cells (if necessary)
  cells_df <- randomlySubsetCells(cells_df, input[["overview_percentage_cells_to_show"]])

  ## put rows in random order
  cells_df <- cells_df[ sample(1:nrow(cells_df)) , ]

  ## get colors for groups
  colors_for_groups <- assignColorsToGroups(cells_df, input[["overview_point_color"]])

  ## prepare hover info
  hover_info <- buildHoverInfoForProjections(cells_df)

  ## check if projection consists of 3 or 2 dimensions
  ## ... selected projection contains 3 dimensions
  if ( ncol(getProjection(projection_to_display)) == 3 ) {

    ## check if selected coloring variable is categorical or numeric
    ## ... selected coloring variable is numeric
    if ( is.numeric(cells_df[[ input[["overview_point_color"]] ]]) ) {
      plot <- plotly::plot_ly(
          cells_df,
          x = ~cells_df[,1],
          y = ~cells_df[,2],
          z = ~cells_df[,3],
          type = "scatter3d",
          mode = "markers",
          marker = list(
            colorbar = list(
              title = input[["overview_point_color"]]
            ),
            color = ~cells_df[[ input[["overview_point_color"]] ]],
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
          text = ~hover_info,
          source = "overview_projection"
        )

    ## ... selected coloring variable is not numeric
    } else {
      plot <- plotly::plot_ly(
          cells_df,
          x = ~cells_df[,1],
          y = ~cells_df[,2],
          z = ~cells_df[,3],
          color = ~cells_df[[ input[["overview_point_color"]] ]],
          colors = colors_for_groups,
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
          text = ~hover_info,
          source = "overview_projection"
        )
    }

    ## add layout to plot
    plot <- plot %>%
      plotly::layout(
        scene = list(
          xaxis = list(
            title = colnames(cells_df)[1],
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = colnames(cells_df)[2],
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          ),
          zaxis = list(
            title = colnames(cells_df)[3],
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
    if ( is.numeric(cells_df[[ input[["overview_point_color"]] ]]) ) {
      plot <- plotly::plot_ly(
        cells_df,
        x = ~cells_df[,1],
        y = ~cells_df[,2],
        type = "scatter",
        mode = "markers",
        marker = list(
          colorbar = list(
            title = input[["overview_point_color"]]
          ),
          color = ~cells_df[[ input[["overview_point_color"]] ]],
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
        text = ~hover_info,
        source = "overview_projection"
      )

    ## ... selected coloring variable is not numeric
    } else {
      plot <- plotly::plot_ly(
        cells_df,
        x = ~cells_df[,1],
        y = ~cells_df[,2],
        color = ~cells_df[[ input[["overview_point_color"]] ]],
        colors = colors_for_groups,
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
        text = ~hover_info,
        source = "overview_projection"
      )
    }

    ## add layout to plot
    plot <- plot %>%
      plotly::layout(
        xaxis = list(
          title = colnames(cells_df)[1],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = input[["overview_scale_x_manual_range"]]
        ),
        yaxis = list(
          title = colnames(cells_df)[2],
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
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

overview_projection_info <- list(
  title = "Dimensional reduction",
  text = HTML("
    Interactive projection of cells into 2-dimensional space based on their expression profile.
    <ul>
      <li>Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44).</li>
      <li>Cells can be colored by the sample they came from, the cluster they were assigned, the number of transcripts or expressed genes, percentage of mitochondrial and ribosomal gene expression, an apoptotic score (calculated based on the expression of few marker genes; more info in the 'Sample info' tab on the left), or cell cycle status (determined using the Seurat and Cyclone method).</li>
      <li>Confidence ellipses show the 95% confidence regions.</li>
      <li>Samples and clusters can be removed from the plot individually to highlight a contrast of interest.</li>
      <li>By default, the point size is set to 15 without any transparency but both these attributes can be changed using the sliders on the left. The point size can also be set to reflect the number of transcripts or expressed genes.</li>
      <li>The last two slider elements on the left can be used to resize the projection axes. This can be particularly useful when a projection contains a population of cell that is very far away from the rest and therefore creates a big empty space (which is not uncommon for UMAPs)</li>
    </ul>
    The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow."
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
  shinyFiles::shinyFileSave(
    input,
    id = "overview_projection_export",
    roots = available_storage_volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  ## retrieve info from dialog
  save_file_input <- shinyFiles::parseSavePath(available_storage_volumes, input[["overview_projection_export"]])

  ## only proceed if a path has been provided
  if ( nrow(save_file_input) > 0 ) {

    ## extract specified file path
    save_file_path <- as.character(save_file_input$datapath[1])

    ## ggplot2 functions are necessary to create the plot
    require("ggplot2")

    ## get selected projection
    projection_to_display <- input[["overview_projection_to_display"]]

    ## merge cell positions in projection and meta data
    cells_df <- cbind(getProjection(projection_to_display), getMetaData())

    ## randomly remove cells (if necessary)
    cells_df <- randomlySubsetCells(cells_df, input[["overview_percentage_cells_to_show"]])

    ## put rows in random order
    cells_df <- cells_df[ sample(1:nrow(cells_df)) , ]

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

      ## start building the plot
      plot <- ggplot(
          cells_df,
          aes_q(
            x = as.name(colnames(cells_df)[1]),
            y = as.name(colnames(cells_df)[2]),
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
        lims(x = xlim, y = ylim) +
        theme_bw()

      ## depending on type of cell coloring, add different color scale
      ## ... categorical
      if (
        is.factor(cells_df[[ input[["overview_point_color"]] ]]) ||
        is.character(cells_df[[ input[["overview_point_color"]] ]])
      ) {

        ## get colors for groups
        colors_for_groups <- assignColorsToGroups(cells_df, input[["overview_point_color"]])

        ## add color assignments
        plot <- plot + scale_fill_manual(values = colors_for_groups)

      ## ... not categorical (probably numerical)
      } else {

        ## add continuous color scale
        plot <- plot +
          scale_fill_distiller(
            palette = "YlGnBu",
            direction = 1,
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
          )
      }

      ## save plot
      pdf(NULL)
      ggsave(save_file_path, plot, height = 8, width = 11)

      ## check if file was succesfully saved
      ## ... successful
      if ( file.exists(save_file_path) ) {

        ## give positive message
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Success!",
          text = paste0("Plot saved successfully as: ", save_file_path),
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
