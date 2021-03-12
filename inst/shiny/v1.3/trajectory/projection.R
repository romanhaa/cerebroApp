##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Projection.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI elements for plot of projection and input parameters.
##----------------------------------------------------------------------------##

output[["trajectory_projection_UI"]] <- renderUI({

  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  tagList(
    fluidRow(
      column(width = 3, offset = 0, style = "padding: 0px;",
        cerebroBox(
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "trajectory_projection_main_parameters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          uiOutput("trajectory_projection_main_parameters_UI")
        ),
        cerebroBox(
          title = tagList(
            "Additional parameters",
            actionButton(
              inputId = "trajectory_projection_additional_parameters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          uiOutput("trajectory_projection_additional_parameters_UI"),
          collapsed = TRUE
        ),
        cerebroBox(
          title = tagList(
            "Group filters",
            actionButton(
              inputId = "trajectory_projection_group_filters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          uiOutput("trajectory_projection_group_filters_UI"),
          collapsed = TRUE
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
              style = "margin-right: 3px"
            ),
            shinyFiles::shinySaveButton(
              "trajectory_projection_export",
              label = "export to PDF",
              title = "Export trajectory to PDF file.",
              filetype = "pdf",
              viewtype = "icon",
              class = "btn-xs"
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
## UI elements for main parameters of projection plot.
##----------------------------------------------------------------------------##

output[["trajectory_projection_main_parameters_UI"]] <- renderUI({
  selectInput(
    "trajectory_point_color",
    label = "Color cells by",
    choices = c(
      "state", "pseudotime",
      colnames(getMetaData())[! colnames(getMetaData()) %in% c("cell_barcode")]
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_projection_main_parameters_info"]], {
  showModal(
    modalDialog(
      trajectory_projection_main_parameters_info$text,
      title = trajectory_projection_main_parameters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_main_parameters_info <- list(
  title = "Main parameters for projection of trajectory",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Color cells by:</b> Select which variable, categorical or continuous, from the meta data should be used to color the cells.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## UI elements for additional parameters of projection plot.
##----------------------------------------------------------------------------##

output[["trajectory_projection_additional_parameters_UI"]] <- renderUI({

  tagList(
    sliderInput(
      "trajectory_point_size",
      label = "Point size",
      min = preferences[["scatter_plot_point_size"]][["min"]],
      max = preferences[["scatter_plot_point_size"]][["max"]],
      step = preferences[["scatter_plot_point_size"]][["step"]],
      value = preferences[["scatter_plot_point_size"]][["default"]]
    ),
    sliderInput(
      "trajectory_point_opacity",
      label = "Point opacity",
      min = preferences[["scatter_plot_point_opacity"]][["min"]],
      max = preferences[["scatter_plot_point_opacity"]][["max"]],
      step = preferences[["scatter_plot_point_opacity"]][["step"]],
      value = preferences[["scatter_plot_point_opacity"]][["default"]]
    ),
    sliderInput(
      "trajectory_percentage_cells_to_show",
      label = "Show % of cells",
      min = preferences[["scatter_plot_percentage_cells_to_show"]][["min"]],
      max = preferences[["scatter_plot_percentage_cells_to_show"]][["max"]],
      step = preferences[["scatter_plot_percentage_cells_to_show"]][["step"]],
      value = preferences[["scatter_plot_percentage_cells_to_show"]][["default"]]
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "trajectory_projection_additional_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_projection_additional_parameters_info"]], {
  showModal(
    modalDialog(
      trajectory_projection_additional_parameters_info$text,
      title = trajectory_projection_additional_parameters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_additional_parameters_info <- list(
  title = "Additional parameters for projection of trajectory",
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
## UI elements for group filters of projection plot.
##----------------------------------------------------------------------------##

output[["trajectory_projection_group_filters_UI"]] <- renderUI({
  group_filters <- list()
  for ( i in getGroups() ) {
    group_filters[[i]] <- shinyWidgets::pickerInput(
      paste0("trajectory_projection_group_filter_", i),
      label = i,
      choices = getGroupLevels(i),
      selected = getGroupLevels(i),
      options = list("actions-box" = TRUE),
      multiple = TRUE
    )
  }
  group_filters
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "trajectory_projection_group_filters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_projection_group_filters_info"]], {
  showModal(
    modalDialog(
      trajectory_projection_group_filters_info$text,
      title = trajectory_projection_group_filters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_group_filters_info <- list(
  title = "Group filters for projection of trajectory",
  text = HTML("
    The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that are pass all filters (for each grouping variable) are shown in the projection.
    "
  )
)

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

  ## build data frame with data
  cells_df <- cbind(trajectory_data[["meta"]], getMetaData()) %>%
    dplyr::filter(!is.na(pseudotime))

  ## available group filters
  group_filters <- names(input)[grepl(names(input), pattern = 'trajectory_projection_group_filter_')]

  ## remove cells based on group filters
  for ( i in group_filters ) {
    group <- strsplit(i, split = 'trajectory_projection_group_filter_')[[1]][2]
    if ( group %in% colnames(cells_df) ) {
      cells_df <- cells_df[which(cells_df[[group]] %in% input[[i]] ),]
    }
  }

  ## randomly remove cells (if necessary)
  cells_df <- randomlySubsetCells(cells_df, input[["trajectory_percentage_cells_to_show"]])

  ## put rows in random order
  cells_df <- cells_df[ sample(1:nrow(cells_df)) , ]

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

  ## prepare hover info
  hover_info <- buildHoverInfoForProjections(cells_df)

  ## add expression levels to hover info
  hover_info <- glue::glue(
    "{hover_info}
    <b>State</b>: {cells_df$state}
    <b>Pseudotime</b>: {formatC(cells_df$pseudotime, format = 'f', digits = 2)}"
  )

  ##
  if (
    is.factor(cells_df[[ input[["trajectory_point_color"]] ]]) ||
    is.character(cells_df[[ input[["trajectory_point_color"]] ]])
  ) {

    ## get colors for groups
    colors_for_groups <- assignColorsToGroups(cells_df, input[["trajectory_point_color"]])

    ##
    plot <- plotly::plot_ly(
      cells_df,
      x = ~DR_1,
      y = ~DR_2,
      color = ~cells_df[[ input[["trajectory_point_color"]] ]],
      colors = colors_for_groups,
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
      text = ~hover_info,
      source = "trajectory_projection"
    )

  ##
  } else {

    ##
    plot <- plotly::plot_ly(
      data = cells_df,
      x = ~DR_1,
      y = ~DR_2,
      type = "scatter",
      mode = "markers",
      marker = list(
        colorbar = list(
          title = colnames(cells_df)[which(colnames(cells_df) == input[["trajectory_point_color"]])]
        ),
        color = ~cells_df[[ input[["trajectory_point_color"]] ]],
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
      text = ~hover_info,
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
        range = range(cells_df$DR_1) * 1.1
      ),
      yaxis = list(
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = range(cells_df$DR_2) * 1.1
      ),
      hoverlabel = list(
        font = list(
          size = 11
        ),
        align = 'left'
      )
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
      footer = NULL,
      size = "l"
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
    input[["trajectory_selected_name"]],
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_point_color"]],
    input[["trajectory_point_size"]],
    input[["trajectory_point_opacity"]]
  )

  ## check selection
  ## ... selection has not been made or there is no cell in it
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
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]],
    input[["trajectory_point_color"]],
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_point_size"]],
    input[["trajectory_point_opacity"]]
  )

  ## open dialog to select where plot should be saved and how the file should
  ## be named
  shinyFiles::shinyFileSave(
    input,
    id = "trajectory_projection_export",
    roots = available_storage_volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  ## retrieve info from dialog
  save_file_input <- shinyFiles::parseSavePath(available_storage_volumes, input[["trajectory_projection_export"]])

  ## only proceed if a path has been provided
  if ( nrow(save_file_input) > 0 ) {

    ## extract specified file path
    save_file_path <- as.character(save_file_input$datapath[1])

    ## ggplot2 functions are necessary to create the plot
    require("ggplot2")

    trajectory_data <- getTrajectory(
      input[["trajectory_selected_method"]],
      input[["trajectory_selected_name"]]
    )

    ## build data frame with data
    cells_df <- cbind(trajectory_data[["meta"]], getMetaData()) %>%
      dplyr::filter(!is.na(pseudotime))

    ## randomly remove cells (if necessary)
    cells_df <- randomlySubsetCells(cells_df, input[["trajectory_percentage_cells_to_show"]])

    ## put rows in random order
    cells_df <- cells_df[ sample(1:nrow(cells_df)) , ]

    ## start building the plot
    plot <- ggplot() +
      geom_point(
        data = cells_df,
        aes_string(
          x = colnames(cells_df)[1],
          y = colnames(cells_df)[2],
          fill = input[["trajectory_point_color"]]
        ),
        shape = 21,
        size = input[["trajectory_point_size"]]/3,
        stroke = 0.2,
        color = "#c4c4c4",
        alpha = input[["trajectory_point_opacity"]]
      ) +
      geom_segment(
        data = trajectory_data[["edges"]],
        aes(
          source_dim_1,
          source_dim_2,
          xend = target_dim_1,
          yend = target_dim_2
        ),
        size = 0.75, linetype = "solid", na.rm = TRUE
      ) +
      theme_bw()

    ## depending on type of cell coloring, add different color scale
    ## ... categorical
    if (
      is.factor(cells_df[[ input[["trajectory_point_color"]] ]]) ||
      is.character(cells_df[[ input[["trajectory_point_color"]] ]])
    ) {

      ## get colors for groups
      colors_for_groups <- assignColorsToGroups(cells_df, input[["trajectory_point_color"]])

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
})
