##----------------------------------------------------------------------------##
## Tab: Marker genes
##----------------------------------------------------------------------------##

## TODO: update description
marker_genes_info <- list(
  title = "Marker genes",
  text = p("Shown here are the marker genes identified for each group - resembling bulk RNA-seq. These genes should help to identify the cell type in this sample or find new markers to purify it. In this analysis, each group is compared to all other samples combined. Only genes with a positive average log-fold change of at least 0.25 are reported - meaning only over-expressed genes are shown. Also, marker genes must be expressed in at least 70% of the cells of the respective sample. Statistical analysis is performed using a classical t-test as it has been shown to be very accurate in single cell RNA-seq. Finally, if data is available, the last column reports for each gene if it is associated with gene ontology term GO:0009986 which is an indicator that the respective gene is present on the cell surface (which could make it more interesting to purify a given population).")
)
