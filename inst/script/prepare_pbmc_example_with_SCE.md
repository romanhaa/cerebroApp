# How to generate the SCE PBMC example data set used in the examples parts of cerebroApp function

```r
library(SingleCellExperiment)
library(scran)
library(scater)
library(cerebroApp)
library(dplyr)

## load counts
pbmc_counts <- read.table(
    file = system.file('extdata', 'pbmc_raw.txt', package = 'Seurat'),
    as.is = TRUE
  ) %>%
  as.matrix()

## create SCE object
pbmc <- SingleCellExperiment(assays = list(counts = pbmc_counts))

## calculate nUMI and nGene
pbmc$nUMI <- colSums(counts(pbmc))
pbmc$nGene <- colSums(counts(pbmc) != 0)

## add sample meta data
colData(pbmc)$sample <- factor('A', levels = 'A')

## log-normalize counts and perform PCA
pbmc <- logNormCounts(pbmc)
pbmc <- runPCA(pbmc)

## cluster cells
SNN_graph <- buildSNNGraph(pbmc, use.dimred = 'PCA')
cluster <- igraph::cluster_walktrap(SNN_graph)$membership
pbmc$cluster <- factor(cluster)

## calculate UMAP
pbmc <- runUMAP(pbmc)

saveRDS(pbmc, 'inst/extdata/v1.3/pbmc_SCE.rds')
```
