##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##

groups_tree_info <- list(
  title = "Relationship tree",
  text = p("The relationship tree reflects the similarity of groups based on their expression profiles. Instead of using the expression values, the correlation is calculated using the user-specified number of principal components (see 'Analysis info' tab on the left).")
)

## TODO: update description
groups_by_other_group_info <- list(
  title = "Samples by cluster",
  text = p("Percentage bar plot representation of the table shown above. Allows to see which groups contribute most strongly to each sample. Groups can be removed from the plot by clicking on them in the legend.")
)

groups_nUMI_info <- list(
  title = "Number of transcripts",
  text = p("Violin plot of the number of transcripts (UMIs) found in each group.")
)

groups_nGene_info <- list(
  title = "Number of expressed genes",
  text = p("Violin plot of the number of expressed genes found in each group.")
)

groups_percent_mt_info <- list(
  title = "Mitochondrial gene expression",
  text = p("Violin plot of the percentage of mitochondrial gene expression found in each group. This reflects the contribution of mitochondrial transcripts to the entire transcriptome in each cell. A list of all genes considered to be mitochondrial can be found in the 'Analysis info' tab on the left.")
)

groups_percent_ribo_info <- list(
  title = "Ribosomal gene expression",
  text = p("Violin plot of the percentage of ribosomal gene expression found in each group. This reflects the contribution of ribosomal transcripts to the entire transcriptome in each cell. A list of all genes considered to be ribosomal can be found in the 'Analysis info' tab on the left.")
)

## TODO: update description
groups_by_cell_cycle_info <- list(
  title = "Cell cycle analysis",
  text = p("Cell cycle distribution by sample using the method embedded in the Seurat framework. For each cell, it calculates scores for both G2M and S phase based on lists of genes (see 'Analysis info' tab on the left) and assigns the cell cycle phase on the basis of these scores.")
)
