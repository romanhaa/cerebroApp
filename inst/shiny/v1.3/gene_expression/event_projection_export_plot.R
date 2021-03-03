##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_projection_export"]], {
  req(expression_projection_data_to_plot())
  ## assign input to variables
  input_data <- expression_projection_data_to_plot()
  cells_df <- input_data[['cells_df']]
  coordinates <- input_data[['coordinates']]
  expression_levels <- input_data[['expression_levels']]
  cells_df$level <- expression_levels
  cells_df <- bind_cols(coordinates, cells_df)
  plot_parameters <- input_data[['plot_parameters']]
  color_settings <- input_data[['color_settings']]
  trajectory <- input_data[['trajectory']]
  ## open dialog to select where plot should be saved and how the file should
  ## be named
  shinyFiles::shinyFileSave(
    input,
    id = "expression_projection_export",
    roots = available_storage_volumes,
    session = session,
    restrictions = system.file(package = "base")
  )
  ## retrieve info from dialog
  save_file_input <- shinyFiles::parseSavePath(
    available_storage_volumes,
    input[["expression_projection_export"]]
  )
  ## only proceed if a path has been provided
  req(nrow(save_file_input) > 0)
  ## make ggplot2 functions available
  require("ggplot2")
  ## extract specified file path
  save_file_path <- as.character(save_file_input$datapath[1])
  ## bring cells in order, either random or highest expression on top
  if (plot_parameters[['plot_order']]=='Random') {
    cell_order <- sample(1:length(expression_levels))
    cells_df <- cells_df[cell_order,]
  } else if (plot_parameters[['plot_order']]=='Highest expression on top') {
    cell_order <- order(expression_levels)
    cells_df <- cells_df[cell_order,]
  }
  ##
  if (
    is.list(expression_levels) ||
    ncol(coordinates) == 3
  ) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Sorry!",
      text = HTML("
        The plot could not be exported to a PDF file.<br>
        Possible reasons:
        <ul>
          <li>You have selected a 3D dimensional reduction.</li>
          <li>Expression is shown in a separate panel per gene.</li>
        </ul>
        Please use the PNG export button in the top-right corner of the projection instead."
      ),
      type = "error",
      html = TRUE
    )
  } else {
    ## check if projection or trajectory should be shown
    ## ... projection
    if ( plot_parameters[["projection"]] %in% availableProjections() ) {
      ## ... separate panels requested and "gene" column present
      if (
        input[["expression_projection_genes_in_separate_panels"]] == TRUE &&
        "gene" %in% colnames(cells_df) == TRUE
      ) {
        ## prepare plot
        plot <- pltExpProj2DMultPanExp(
          df = cells_df,
          point_size = plot_parameters[["point_size"]],
          point_opacity = plot_parameters[["point_opacity"]],
          point_border = plot_parameters[["draw_border"]],
          color_scale = color_settings[["color_scale"]],
          color_range = color_settings[["color_range"]],
          x_range = plot_parameters[["x_range"]],
          y_range = plot_parameters[["y_range"]]
        )
      } else {
        ## prepare plot
        plot <- pltExpProj2DSglPanExp(
          df = cells_df,
          point_size = plot_parameters[["point_size"]],
          point_opacity = plot_parameters[["point_opacity"]],
          point_border = plot_parameters[["draw_border"]],
          color_scale = color_settings[["color_scale"]],
          color_range = color_settings[["color_range"]],
          x_range = plot_parameters[["x_range"]],
          y_range = plot_parameters[["y_range"]]
        )
      }
    ## ... trajectory
    } else {
      ## prepare plot
      plot <- pltExpTrj2DSglPanExp(
        df = cells_df,
        trajectory_edges = trajectory[["edges"]],
        point_size = plot_parameters[["point_size"]],
        point_opacity = plot_parameters[["point_opacity"]],
        point_border = plot_parameters[["draw_border"]],
        color_scale = color_settings[["color_scale"]],
        color_range = color_settings[["color_range"]],
        x_range = plot_parameters[["x_range"]],
        y_range = plot_parameters[["y_range"]]
      )
    }
    ## plot must be a ggplot object, otherwise don't proceed
    req(is.ggplot(plot))
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
