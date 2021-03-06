##----------------------------------------------------------------------------##
## Plotly plot of the selected projection.
##----------------------------------------------------------------------------##
output[["overview_projection"]] <- renderPlot({
  req(overview_projection_data_to_plot())
  require('scattermore')
  cells_df <- overview_projection_data_to_plot()[['cells_df']]
  coordinates <- overview_projection_data_to_plot()[['coordinates']]
  plot_parameters <- overview_projection_data_to_plot()[['plot_parameters']]
  color_assignments <- overview_projection_data_to_plot()[['color_assignments']]
  variable_to_color_cells <- plot_parameters[["color_variable"]]
  ## check if selection projection consists of 2 or 3 dimensions
  ## ... selection projection consists of 2 dimensions
  if ( plot_parameters[['n_dimensions']] == 2 ) {
    ## start building the plot
    plot <- ggplot(
        cbind(coordinates, cells_df),
        aes_q(
          x = as.name(colnames(coordinates)[1]),
          y = as.name(colnames(coordinates)[2]),
          color = as.name(variable_to_color_cells)
        )
      ) +
      geom_scattermore(
        pointsize = plot_parameters[["point_size"]]/2,
        alpha = plot_parameters[["point_opacity"]],
        pixels = c(700,700)
      ) +
      coord_cartesian(
        xlim = overview_projection_ranges$x,
        ylim = overview_projection_ranges$y
      ) +
      theme_bw()
    ## depending on type of cell coloring, add different color scale
    ## ... categorical
    if (
      is.factor(cells_df[[ variable_to_color_cells ]]) ||
      is.character(cells_df[[ variable_to_color_cells ]])
    ) {
      ## add color assignments
      plot <- plot + scale_color_manual(values = color_assignments) +
        guides(color = guide_legend(override.aes = list(size=4)))
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
        scale_color_distiller(
          palette = "YlGnBu",
          direction = 1,
          guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
        )
    }
    return(plot)
  } else {
    ggplot()
  }
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
