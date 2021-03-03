##----------------------------------------------------------------------------##
## Plotly plot of the selected projection.
##----------------------------------------------------------------------------##
output[["overview_projection"]] <- plotly::renderPlotly({
  plotly::plot_ly(type = 'scattergl', mode = 'markers', source = "overview_projection") %>%
  plotly::layout(
    xaxis = list(
      autorange = TRUE,
      mirror = TRUE,
      showline = TRUE,
      zeroline = FALSE
    ),
    yaxis = list(
      autorange = TRUE,
      mirror = TRUE,
      showline = TRUE,
      zeroline = FALSE
    )
  )
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
