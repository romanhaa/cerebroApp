# How to generate the pre-loaded example data set for the Cerebro user interface

## Download raw data

Download raw data from Cerebro GitHub repository: <https://github.com/romanhaa/Cerebro/blob/master/examples/pbmc_10k_v3/raw_data/filtered_feature_bc_matrix.h5>

## Preparation

```r
library(dplyr)
library(Seurat)
library(SingleR)
library(monocle)
library(cerebroApp)

options(width = 100)
set.seed(1234567)
```

## Load and randomly downsample data to 501 cells

```r
feature_matrix <- Read10X_h5('~/Downloads/filtered_feature_bc_matrix.h5')
feature_matrix <- feature_matrix[ , sample(1:ncol(feature_matrix), 501) ]
```

## Basic Seurat workflow

```r
seurat <- CreateSeuratObject(
  project = 'pbmc_10k_v3',
  counts = feature_matrix,
  min.cells = 10
)
seurat <- subset(seurat, subset = nCount_RNA >= 100 & nFeature_RNA >= 50)
seurat <- NormalizeData(seurat)
seurat <- FindVariableFeatures(seurat)
seurat <- ScaleData(seurat, vars.to.regress = 'nCount_RNA')
seurat <- RunPCA(seurat, npcs = 30, features = seurat@assays$RNA@var.features)
```

## Clustering

```r
seurat <- FindNeighbors(seurat)
seurat <- FindClusters(seurat, resolution = 0.5)
seurat@meta.data$RNA_snn_res.0.5 <- NULL
```

## Randomly assign cells to samples

```r
sample_info <- rep(NA, 501)
sample_info[1:166] <- 'pbmc_10k_v3_rep1'
sample_info[167:334] <- 'pbmc_10k_v3_rep2'
sample_info[335:501] <- 'pbmc_10k_v3_rep3'
sample_info <- factor(sample_info, levels = c('pbmc_10k_v3_rep1','pbmc_10k_v3_rep2','pbmc_10k_v3_rep3'))
seurat@meta.data$sample <- sample_info
```

## Cell cycle analysis

```r
seurat <- CellCycleScoring(
seurat,
  g2m.features = cc.genes$g2m.genes,
  s.features = cc.genes$s.genes
)

seurat@misc$gene_lists$G2M_phase_genes <- cc.genes$g2m.genes
seurat@misc$gene_lists$S_phase_genes <- cc.genes$s.genes
```

## Dimensional reduction

```r
seurat <- RunTSNE(
  seurat,
  reduction.name = 'tSNE',
  reduction.key = 'tSNE_',
  dims = 1:30,
  dim.embed = 2,
  perplexity = 30,
  seed.use = 100
)

seurat <- RunTSNE(
  seurat,
  reduction.name = 'tSNE_3D',
  reduction.key = 'tSNE3D_',
  dims = 1:30,
  dim.embed = 3,
  perplexity = 30,
  seed.use = 100
)

seurat <- RunUMAP(
  seurat,
  reduction.name = 'UMAP',
  reduction.key = 'UMAP_',
  dims = 1:30,
  n.components = 2,
  seed.use = 100
)

seurat <- RunUMAP(
  seurat,
  reduction.name = 'UMAP_3D',
  reduction.key = 'UMAP3D_',
  dims = 1:30,
  n.components = 3,
  seed.use = 100
)
```

## Curate meta data

```r
seurat@misc$experiment <- list(
  experiment_name = 'pbmc_10k_v3',
  organism = 'hg',
  date_of_analysis = Sys.Date()
)

seurat@misc$parameters <- list(
  gene_nomenclature = 'gene_name',
  discard_genes_expressed_in_fewer_cells_than = 10,
  keep_mitochondrial_genes = TRUE,
  variables_to_regress_out = 'nUMI',
  number_PCs = 30,
  cluster_resolution = 0.5,
  tSNE_perplexity = 30
)

seurat@misc$parameters$filtering <- list(
  UMI_min = 100,
  UMI_max = Inf,
  genes_min = 50,
  genes_max = Inf
)

seurat@misc$technical_info$cerebroApp_version <- utils::packageVersion('cerebroApp')
seurat@misc$technical_info$Seurat <- utils::packageVersion('Seurat')
seurat@misc$technical_info <- list(
  'R' = capture.output(devtools::session_info())
)
```

## Assign cell types

```r
singler_ref <- BlueprintEncodeData()

singler_results_blueprintencode_main <- SingleR(
  test = GetAssayData(seurat, assay = 'RNA', slot = 'data'),
  ref = singler_ref,
  labels = singler_ref@colData@listData$label.main
)

seurat@meta.data$cell_type_singler_blueprintencode_main <- singler_results_blueprintencode_main@listData$labels

singler_scores <- singler_results_blueprintencode_main@listData$scores %>%
  as_tibble() %>%
  dplyr::mutate(assigned_score = NA)

for ( i in seq_len(nrow(singler_scores)) ) {
  singler_scores$assigned_score[i] <- singler_scores[[singler_results_blueprintencode_main@listData$labels[i]]][i]
}

seurat@meta.data$cell_type_singler_blueprintencode_main_score <- singler_scores$assigned_score
```

## Calculate relationship trees

### Samples

```r
Idents(seurat) <- "sample"
seurat <- BuildClusterTree(
  seurat,
  dims = 1:30,
  reorder = FALSE,
  reorder.numeric = FALSE
)

seurat@misc$trees$sample <- seurat@tools$BuildClusterTree
```

### Clusters

```r
Idents(seurat) <- "seurat_clusters"
seurat <- BuildClusterTree(
  seurat,
  dims = 1:30,
  reorder = FALSE,
  reorder.numeric = FALSE
)

seurat@misc$trees$seurat_clusters <- seurat@tools$BuildClusterTree
```

### Cell types

```r
Idents(seurat) <- "cell_type_singler_blueprintencode_main"
seurat <- BuildClusterTree(
  seurat,
  dims = 1:30,
  reorder = FALSE,
  reorder.numeric = FALSE,
  verbose = FALSE
)

seurat@misc$trees$cell_type_singler_blueprintencode_main <- seurat@tools$BuildClusterTree
```

## cerebroApp functions

```r
seurat <- addPercentMtRibo(
  seurat,
  organism = 'hg',
  gene_nomenclature = 'name'
)

seurat <- getMostExpressedGenes(
  seurat,
  assay = 'RNA',
  groups = c('sample','seurat_clusters','cell_type_singler_blueprintencode_main')
)

seurat <- getMarkerGenes(
  seurat,
  assay = 'RNA',
  organism = 'hg',
  groups = c('sample','seurat_clusters','cell_type_singler_blueprintencode_main'),
  name = 'cerebro_seurat',
  only_pos = TRUE
)

seurat <- getEnrichedPathways(
  seurat,
  marker_genes_input = 'cerebro_seurat',
  adj_p_cutoff = 0.01,
  max_terms = 100
)

example_gene_set <- system.file("extdata/example_gene_set.gmt", package = "cerebroApp")

seurat <- performGeneSetEnrichmentAnalysis(
  seurat,
  assay = 'RNA',
  GMT_file = example_gene_set,
  groups = c('sample','seurat_clusters','cell_type_singler_blueprintencode_main')
)
```

## Trajectory analysis with Monocle

### All cells

```r
monocle_all_cells <- newCellDataSet(
  seurat@assays$RNA@counts,
  phenoData = new('AnnotatedDataFrame', data = seurat@meta.data),
  featureData = new('AnnotatedDataFrame', data = data.frame(
    gene_short_name = rownames(seurat@assays$RNA@counts),
    row.names = rownames(seurat@assays$RNA@counts))
  )
)

monocle_all_cells <- estimateSizeFactors(monocle_all_cells)
monocle_all_cells <- estimateDispersions(monocle_all_cells)
monocle_all_cells <- setOrderingFilter(monocle_all_cells, seurat@assays$RNA@var.features)
monocle_all_cells <- reduceDimension(monocle_all_cells, max_components = 2, method = 'DDRTree')
monocle_all_cells <- orderCells(monocle_all_cells)

seurat <- extractMonocleTrajectory(monocle_all_cells, seurat, 'all_cells')
```

### Cells in G1 phase

```r
G1_cells <- which(seurat@meta.data$Phase == 'G1')

monocle_subset_of_cells <- newCellDataSet(
  seurat@assays$RNA@counts[,G1_cells],
  phenoData = new('AnnotatedDataFrame', data = seurat@meta.data[G1_cells,]),
  featureData = new('AnnotatedDataFrame', data = data.frame(
    gene_short_name = rownames(seurat@assays$RNA@counts),
    row.names = rownames(seurat@assays$RNA@counts))
  )
)

monocle_subset_of_cells <- estimateSizeFactors(monocle_subset_of_cells)
monocle_subset_of_cells <- estimateDispersions(monocle_subset_of_cells)
monocle_subset_of_cells <- setOrderingFilter(monocle_subset_of_cells, seurat@assays$RNA@var.features)
monocle_subset_of_cells <- reduceDimension(monocle_subset_of_cells, max_components = 2, method = 'DDRTree')
monocle_subset_of_cells <- orderCells(monocle_subset_of_cells)

seurat <- extractMonocleTrajectory(monocle_subset_of_cells, seurat, 'subset_of_cells')
```

## Randomly downsample genes to 1000

```r
seurat@assays$RNA@data <- seurat@assays$RNA@data[ sample(1:nrow(seurat@assays$RNA@data), 1000) , ]
```

## Export to Cerebro format

```r
exportFromSeurat(
  seurat,
  assay = 'RNA',
  slot = 'data',
  file = 'inst/extdata/v1.3/example.crb',
  experiment_name = 'pbmc_10k_v3',
  organism = 'hg',
  columns_groups = c('sample','seurat_clusters','cell_type_singler_blueprintencode_main'),
  column_nUMI = 'nCount_RNA',
  column_nGene = 'nFeature_RNA',
  columns_cell_cycle = c('Phase'),
  add_all_meta_data = TRUE
)
```
