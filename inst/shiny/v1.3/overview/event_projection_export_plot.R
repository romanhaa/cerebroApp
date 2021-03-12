##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_projection_export"]], {
  req(overview_projection_data_to_plot())
  input_data <- overview_projection_data_to_plot()
  cells_df <- input_data[['cells_df']]
  coordinates <- input_data[['coordinates']]
  plot_parameters <- input_data[['plot_parameters']]
  color_assignments <- input_data[['color_assignments']]
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
  save_file_input <- shinyFiles::parseSavePath(
    available_storage_volumes,
    input[["overview_projection_export"]]
  )
  ## only proceed if a path has been provided
  req(nrow(save_file_input) > 0)
  ## ggplot2 functions are necessary to create the plot
  require("ggplot2")
  ## extract specified file path
  save_file_path <- as.character(save_file_input$datapath[1])
  ##
  variable_to_color_cells <- plot_parameters[["color_variable"]]
  ## check if selection projection consists of 2 or 3 dimensions
  ## ... selection projection consists of 2 dimensions
  if ( plot_parameters[['n_dimensions']] == 2 ) {
    ##
    stroke <- ifelse(plot_parameters[["draw_border"]], 0.2, 0)
    ## start building the plot
    plot <- ggplot(
        cbind(coordinates, cells_df),
        aes_q(
          x = as.name(colnames(coordinates)[1]),
          y = as.name(colnames(coordinates)[2]),
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
        group_labels <- centerOfGroups(coordinates, cells_df, 2, variable_to_color_cells)
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
