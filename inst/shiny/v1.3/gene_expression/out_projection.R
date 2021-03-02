##----------------------------------------------------------------------------##
## Plot of projection.
##----------------------------------------------------------------------------##
output[["expression_projection"]] <- plotly::renderPlotly({
  plotly::plot_ly(type = 'scattergl', mode = 'markers', source = "expression_projection") %>%
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
observeEvent(input[["expression_projection_info"]], {
  showModal(
    modalDialog(
      expression_projection_info$text,
      title = expression_projection_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_projection_info <- list(
  title = "Dimensional reduction",
  text = HTML("
    Interactive projection of cells into two- or three-dimensional space based on their expression profile.<br>
    <ul>
      <li>Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44).</li>
      <li>Cell color reflects the log-normalised expression of entered genes. If more than 1 gene is entered or a gene set is selected, the color reflects the average expression of all genes. Genes must be in separate lines or separated by a space, comma, or semicolon. Reported below the projection are the genes that are present and absent in this data set. Absent genes could either have been annotated with a different name or were not expressed in any of the cells. Matching of gene names is case-insensitive, that means Myc/MYC/myc are treated equally.</li>
      <li>Cells can be plotted either randomly (to give a more unbiased perspective) or in the order of expression (with highest expression plotted last), sometimes resulting in a more appealing figure.</li>
      <li>The last two slider elements on the left can be used to resize the projection axes. This can be particularly useful when a projection contains a population of cell that is very far away from the rest and therefore creates a big empty space (which is not uncommon for UMAPs).</li>
    </ul>
    The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow.<br>
    <h4>Experimental options</h4>
    Experimental options can be accessed from the gear icon next to the 'export to PDF' button.<br>
    <b>Show genes in separate panels</b><br>
    When selecting multiple genes as input (up to 8), this option will show the expression of each gene in a separate panel instead of calculating the mean expression across all genes. The option is labeled as 'experimental' because of its poor implementation:
    <ol>
      <li>Hovering over the cells shows only limited information.</li>
      <li>Cells cannot be shown with a (grey) border around them.</li>
      <li>All genes have the some color scale.</li>
      <li>Cells cannot be selected.</li>
      <li>The 'Expression by group' panel needs to be deactivated.</li>
      <li>It throws annoying but innocent warning messages in the log.</li>
    </ol>
    Yet, this feature might be useful in some situations.
    "
  )
)
