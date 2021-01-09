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
              uiOutput("overview_projection_show_group_label_UI"),
              uiOutput("overview_projection_point_border_UI"),
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
      "overview_projection_point_color",
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
      "overview_projection_point_size",
      label = "Point size",
      min = scatter_plot_point_size[["min"]],
      max = scatter_plot_point_size[["max"]],
      step = scatter_plot_point_size[["step"]],
      value = scatter_plot_point_size[["default"]]
    ),
    sliderInput(
      "overview_projection_point_opacity",
      label = "Point opacity",
      min = scatter_plot_point_opacity[["min"]],
      max = scatter_plot_point_opacity[["max"]],
      step = scatter_plot_point_opacity[["step"]],
      value = scatter_plot_point_opacity[["default"]]
    ),
    sliderInput(
      "overview_projection_percentage_cells_to_show",
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
      options = list(
        "actions-box" = TRUE
      ),
      multiple = TRUE
    )
  }

  group_filters
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_group_filters_UI",
  suspendWhenHidden = FALSE
)

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

##----------------------------------------------------------------------------##
## UI elements with switch to show group labels in projection.
##----------------------------------------------------------------------------##
output[["overview_projection_show_group_label_UI"]] <- renderUI({

  req(
    input[["overview_projection_point_color"]]
  )

  if ( input[["overview_projection_point_color"]] %in% getGroups() ) {
    shinyWidgets::awesomeCheckbox(
      inputId = "overview_projection_show_group_label",
      label = "Show group labels in projection",
      value = TRUE
    )
  }
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_show_group_label_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## UI elements with switch to draw border around cells.
##----------------------------------------------------------------------------##
output[["overview_projection_point_border_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "overview_projection_point_border",
    label = "Draw border around cells",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_point_border_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## UI elements to select X and Y limits in projection.
##----------------------------------------------------------------------------##
output[["overview_projection_scales_UI"]] <- renderUI({

  ##
  if (
    is.null(input[["overview_projection_to_display"]]) ||
    is.na(input[["overview_projection_to_display"]])
  ) {
    projection_to_display <- availableProjections()[1]
  } else {
    projection_to_display <- input[["overview_projection_to_display"]]
  }

  ##
  XYranges <- getXYranges(getProjection(projection_to_display))

  ##
  tagList(
    sliderInput(
      "overview_projection_scale_x_manual_range",
      label = "Range of X axis",
      min = XYranges$x$min,
      max = XYranges$x$max,
      value = c(XYranges$x$min, XYranges$x$max)
    ),
    sliderInput(
      "overview_projection_scale_y_manual_range",
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
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
overview_projection_parameters_plot_raw <- reactive({
  ## require input UI elements
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_point_color"]],
    input[["overview_projection_point_size"]],
    input[["overview_projection_point_opacity"]],
    !is.null(input[["overview_projection_point_border"]]),
    input[["overview_projection_scale_x_manual_range"]],
    input[["overview_projection_scale_y_manual_range"]],
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  ## collect parameters
  parameters <- list(
    projection = input[["overview_projection_to_display"]],
    n_dimensions = ncol(getProjection(input[["overview_projection_to_display"]])),
    color_variable = input[["overview_projection_point_color"]],
    point_size = input[["overview_projection_point_size"]],
    point_opacity = input[["overview_projection_point_opacity"]],
    draw_border = input[["overview_projection_point_border"]],
    group_labels = input[["overview_projection_show_group_label"]],
    x_range = input[["overview_projection_scale_x_manual_range"]],
    y_range = input[["overview_projection_scale_y_manual_range"]],
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  ## return parameters
  return(parameters)
})

overview_projection_parameters_plot <- debounce(overview_projection_parameters_plot_raw, 1)

##----------------------------------------------------------------------------##
## Color assignments.
##----------------------------------------------------------------------------##
overview_projection_color_assignments <- reactive({
  req(
    overview_projection_data(),
    overview_projection_parameters_plot()
  )
  return(
    assignColorsToGroups(
      overview_projection_data(),
      overview_projection_parameters_plot()['color_variable']
    )
  )
})

##----------------------------------------------------------------------------##
## Input parameters for filtering cells.
##----------------------------------------------------------------------------##
overview_projection_parameters_cell_filtering_raw <- reactive({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_percentage_cells_to_show"]]
  )
  ## require group filters UI elements and at least 1 group level to be selected
  for ( i in getGroups() ) {
    req(input[[paste0("overview_projection_group_filter_", i)]])
  }
  parameters <- list(
    projection = input[["overview_projection_to_display"]],
    pct_cells = input[["overview_projection_percentage_cells_to_show"]],
    group_filters = list()
  )
  ## store group filters
  for ( i in getGroups() ) {
    parameters[['group_filters']][[ i ]] <- input[[paste0("overview_projection_group_filter_", i)]]
  }
  return(parameters)
})

overview_projection_parameters_cell_filtering <- debounce(overview_projection_parameters_cell_filtering_raw, 1)

##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
overview_projection_data <- reactive({
  req(overview_projection_parameters_cell_filtering())
  parameters <- overview_projection_parameters_cell_filtering()
  cells_df <- cbind(getProjection(parameters[["projection"]]), getMetaData())
  ## remove cells based on group filters
  for ( i in getGroups() ) {
    ## make sure that group exists in meta data (as column) and that selected
    ## groups are not NULL, then subset the data frame
    if ( i %in% colnames(cells_df) ) {
      cells_df <- cells_df[which(cells_df[[i]] %in% parameters[["group_filters"]][[ i ]] ),]
    }
  }
  ## randomly remove cells (if necessary)
  cells_df <- randomlySubsetCells(cells_df, parameters[["pct_cells"]])
  ## put rows in random order
  cells_df <- cells_df[ sample(1:nrow(cells_df)) , ]
  return(cells_df)
})

##----------------------------------------------------------------------------##
## Hover info.
##----------------------------------------------------------------------------##
overview_projection_hover_info <- reactive({
  req(overview_projection_data())
  cells_df <- overview_projection_data()
  hover_info <- buildHoverInfoForProjections(cells_df)
  hover_info <- setNames(hover_info, cells_df$cell_barcode)
  return(hover_info)
})

##----------------------------------------------------------------------------##
## Plotly plot of the selected projection.
##----------------------------------------------------------------------------##
output[["overview_projection"]] <- plotly::renderPlotly({
  plot_ly(type = 'scattergl', mode = 'markers', source = "overview_projection")
})

##
overview_projection_data_to_plot <- reactive({
  req(
    overview_projection_data(),
    overview_projection_parameters_plot(),
    reactive_colors(),
    overview_projection_hover_info()
  )
  if ( is.numeric(overview_projection_parameters_plot()[['color_variable']]) ) {
    color_assignments <- NA
  } else {
    color_assignments <- assignColorsToGroups(
      overview_projection_data(),
      overview_projection_parameters_plot()[['color_variable']]
    )
  }
  list(
    cells_df = overview_projection_data(),
    plot_parameters = overview_projection_parameters_plot(),
    color_assignments = color_assignments,
    hover_info = overview_projection_hover_info()
  )
})

##
observeEvent(overview_projection_data_to_plot(), {
  req(overview_projection_data_to_plot())
  overview_projection_update_plot(overview_projection_data_to_plot())
})

## function to be executed to update figure
overview_projection_update_plot <- function(input) {
  cells_df <- input[['cells_df']]
  plot_parameters <- input[['plot_parameters']]
  color_assignments <- input[['color_assignments']]
  hover_info <- input[['hover_info']]
  color_input <- cells_df[[ plot_parameters[['color_variable']] ]]
  if ( is.numeric(color_input) ) {
    output_meta <- list(
      color_type = 'continuous',
      traces = plot_parameters[['color_variable']],
      color_variable = plot_parameters[['color_variable']]
    )
    output_data <- list(
      x = cells_df[[1]],
      y = cells_df[[2]],
      color = color_input,
      point_size = plot_parameters[["point_size"]],
      point_opacity = plot_parameters[["point_opacity"]],
      point_line = list()
    )
    if ( plot_parameters[["draw_border"]] ) {
      output_data[['point_line']] <- list(
        color = "rgb(196,196,196)",
        width = 1
      )
    }
    output_hover <- list(
      hoverinfo = ifelse(plot_parameters[["hover_info"]], 'text', 'skip'),
      text = ifelse(plot_parameters[["hover_info"]], unname(hover_info), 'test')
    )
    if ( plot_parameters[['n_dimensions']] == 2 ) {
      shinyjs::js$updatePlot2DContinuous(
        output_meta,
        output_data,
        output_hover
      )
    } else if ( plot_parameters[['n_dimensions']] == 3 ) {
      output_data[['z']] <- cells_df[[3]]
      shinyjs::js$updatePlot3DContinuous(
        output_meta,
        output_data,
        output_hover
      )
    }
  } else {
    output_meta <- list(
      color_type = 'categorical',
      traces = list(),
      color_variable = plot_parameters[['color_variable']]
    )
    output_data <- list(
      x = list(),
      y = list(),
      z = list(),
      color = list(),
      point_size = plot_parameters[["point_size"]],
      point_opacity = plot_parameters[["point_opacity"]],
      point_line = list()
    )
    if ( plot_parameters[["draw_border"]] ) {
      output_data[['point_line']] <- list(
        color = "rgb(196,196,196)",
        width = 1
      )
    }
    output_hover <- list(
      hoverinfo = ifelse(plot_parameters[["hover_info"]], 'text', 'skip'),
      text = ifelse(plot_parameters[["hover_info"]], list(), 'test')
    )
    if ( plot_parameters[['n_dimensions']] == 2 ) {
      i <- 1
      for ( j in names(color_assignments) ) {
        output_meta[['traces']][[i]] <- j
        cells_to_extract <- which(color_input==j)
        output_data[['x']][[i]] <- cells_df[[1]][cells_to_extract]
        output_data[['y']][[i]] <- cells_df[[2]][cells_to_extract]
        output_data[['color']][[i]] <- unname(color_assignments[which(names(color_assignments)==j)])
        if ( plot_parameters[["hover_info"]] ) {
          hover_info_matched <- match(
            cells_df[['cell_barcode']][cells_to_extract],
            names(hover_info)
          )
          output_hover[['text']][[i]] <- unname(hover_info[hover_info_matched])
        }
        i <- i + 1
      }
      group_centers_df <- centerOfGroups(cells_df, 2, plot_parameters[['color_variable']])
      output_group_centers <- list(
        group = group_centers_df[['group']],
        x = group_centers_df[['x_median']],
        y = group_centers_df[['y_median']]
      )
      shinyjs::js$updatePlot2DCategorical(
        output_meta,
        output_data,
        output_hover,
        output_group_centers
      )
    } else if ( plot_parameters[['n_dimensions']] == 3 ) {
      i <- 1
      for ( j in names(color_assignments) ) {
        output_meta[['traces']][[i]] <- j
        cells_to_extract <- which(color_input==j)
        output_data[['x']][[i]] <- cells_df[[1]][cells_to_extract]
        output_data[['y']][[i]] <- cells_df[[2]][cells_to_extract]
        output_data[['z']][[i]] <- cells_df[[3]][cells_to_extract]
        output_data[['color']][[i]] <- unname(color_assignments[which(names(color_assignments)==j)])
        if ( plot_parameters[["hover_info"]] ) {
          hover_info_matched <- match(
            cells_df[['cell_barcode']][cells_to_extract],
            names(hover_info)
          )
          output_hover[['text']][[i]] <- unname(hover_info[hover_info_matched])
        }
        i <- i + 1
      }
      group_centers_df <- centerOfGroups(cells_df, 3, plot_parameters[['color_variable']])
      output_group_centers <- list(
        group = group_centers_df[['group']],
        x = group_centers_df[['x_median']],
        y = group_centers_df[['y_median']],
        z = group_centers_df[['z_median']]
      )
      shinyjs::js$updatePlot3DCategorical(
        output_meta,
        output_data,
        output_hover,
        output_group_centers
      )
    }
  }
}

##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
overview_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(
    overview_projection_data_to_plot()
  )
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if (
    is.null(plotly::event_data("plotly_selected", source = "overview_projection")) ||
    length(plotly::event_data("plotly_selected", source = "overview_projection")) == 0
  ) {
    return(NULL)
  ## ... selection has been made and at least 1 cell is in it
  } else {
    ## get number of selected cells
    plotly::event_data("plotly_selected", source = "overview_projection") %>%
    dplyr::mutate(identifier = paste0(x, '-', y)) %>%
    return()
  }
})

##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##
output[["overview_number_of_selected_cells"]] <- renderText({
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if ( is.null(overview_projection_selected_cells()) ) {
    ## manually set counter to 0
    number_of_selected_cells <- 0
  ## ... selection has been made and at least 1 cell is in it
  } else {
    ## get number of selected cells
    number_of_selected_cells <- overview_projection_selected_cells() %>%
      nrow() %>%
      formatC(format = "f", big.mark = ",", digits = 0)
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
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(overview_projection_data_to_plot())
  ##
  cells_df <- overview_projection_data_to_plot()[['cells_df']]
  plot_parameters <- overview_projection_data_to_plot()[['plot_parameters']]
  color_assignments <- overview_projection_data_to_plot()[['color_assignments']]
  hover_info <- overview_projection_data_to_plot()[['hover_info']]
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
  req(nrow(save_file_input) > 0)
  ## extract specified file path
  save_file_path <- as.character(save_file_input$datapath[1])
  ## ggplot2 functions are necessary to create the plot
  require("ggplot2")
  ## get selected projection
  projection_to_display <- plot_parameters[["projection"]]
  variable_to_color_cells <- plot_parameters[["color_variable"]]
  ## check if selection projection consists of 2 or 3 dimensions
  ## ... selection projection consists of 2 dimensions
  if ( plot_parameters[['n_dimensions']] == 2 ) {
    ##
    stroke <- ifelse(plot_parameters[["draw_border"]], 0.2, 0)
    ## start building the plot
    plot <- ggplot(
        cells_df,
        aes_q(
          x = as.name(colnames(cells_df)[1]),
          y = as.name(colnames(cells_df)[2]),
          fill = as.name(variable_to_color_cells)
        )
      ) +
      geom_point(
        shape = 21,
        size = plot_parameters[["point_size"]]/3,
        stroke = stroke,
        color = "#c4c4c4",
        alpha = plot_parameters[["point_opacity"]]
      ) +
      lims(
        x = plot_parameters[["x_range"]],
        y = plot_parameters[["y_range"]]
      ) +
      theme_bw()
    ## depending on type of cell coloring, add different color scale
    ## ... categorical
    if (
      is.factor(cells_df[[ variable_to_color_cells ]]) ||
      is.character(cells_df[[ variable_to_color_cells ]])
    ) {
      ## add color assignments
      plot <- plot + scale_fill_manual(values = color_assignments)
      ## check if group labels should be plotted and, if so, add them
      if ( plot_parameters[["group_labels"]] == TRUE ) {
        ## calculate group level centers
        group_labels <- centerOfGroups(cells_df, 2, variable_to_color_cells)
        ## add group level labels at center of respective groups
        plot <- plot +
          geom_label(
            data = group_labels,
            mapping = aes(x_median, y_median, label = group),
            fill = 'white',
            size = 4.5,
            color = 'black',
            alpha = 0.5,
            fontface = 'bold',
            label.size = 0,
            show.legend = FALSE
          )
      }
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
  ## ... selection projection consists of 3 dimensions
  } else if ( plot_parameters[['n_dimensions']] == 3 ) {
    ## give error message
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Sorry!",
      text = "It's currently not possible to create PDF plots from 3D dimensional reductions. Please use the PNG export button in the panel or a 2D dimensional reduction instead.",
      type = "error"
    )
  }
})

##----------------------------------------------------------------------------##
## example for implementation of nested checkboxes with shinyTree for selection
## of group levels to show; works similar to cellxgene; anyway decided against
## it because it creates a new dependency and isn't as aesthetically pleasing as
## the existing solution
##----------------------------------------------------------------------------##

# output[["overview_projection_group_filters_tree"]] <- shinyTree::renderTree({
#   groups <- list()
#   for ( i in getGroups() ) {
#     groups[[i]] <- structure(
#       as.list(
#         setNames(
#           getGroupLevels(i),
#           getGroupLevels(i)
#         )
#       ),
#       stselected = TRUE
#     )
#   }
#   groups
# })

# output[["overview_projection_group_filters_selected_groups"]] <- renderPrint({
#   tree <- input[["overview_projection_group_filters_tree"]]
#   req(overview_projection_group_filters_tree)
#   str(shinyTree::get_selected(tree, format = "slices"))
# })

# output[["overview_projection_group_filters_tree_UI"]] <- renderUI({
#   tagList(
#     shinyTree::shinyTree(
#       "overview_projection_group_filters_tree",
#       theme = "proton",
#       themeIcons = FALSE,
#       themeDots = FALSE,
#       checkbox = TRUE
#     ),
#     verbatimTextOutput("sel_slices")
#   )
# })
