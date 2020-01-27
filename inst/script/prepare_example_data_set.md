# How to generate the pre-loaded example data set for the Cerebro user interface

```r
## download the .crb file of the 10k PBMC example from the Cerebro GitHub
## repository
## https://github.com/romanhaa/Cerebro/tree/master/examples
data <- readRDS('cerebro_pbmc_10k_v3_2019-10-10.crb')

data$cells$cell_cycle_cyclone <- data$cells$cell_cycle_seurat

data$cells$sample <- as.character(data$cells$sample)
data$cells$sample[1:5325] <- 'pbmc_10k_v3_rep1'
data$cells$sample[5326:(5325+3871)] <- 'pbmc_10k_v3_rep2'
data$cells$sample[(5325+3871+1):11769] <- 'pbmc_10k_v3_rep3'
data$cells$sample <- factor(data$cells$sample, levels = c('pbmc_10k_v3_rep1','pbmc_10k_v3_rep2','pbmc_10k_v3_rep3'))

data$samples$overview <- tibble(
  sample = c('pbmc_10k_v3_rep1','pbmc_10k_v3_rep2','pbmc_10k_v3_rep3')
)

data$samples$colors <- data$clusters$colors[1:3]
names(data$samples$colors) <- c('pbmc_10k_v3_rep1','pbmc_10k_v3_rep2','pbmc_10k_v3_rep3')

data$samples$by_cluster <- data$cells %>%
  dplyr::group_by(sample, cluster) %>%
  dplyr::summarize(count = dplyr::n()) %>%
  tidyr::spread(cluster, count, fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
  dplyr::select(c('sample', 'total_cell_count', dplyr::everything())) %>%
  dplyr::arrange(factor(sample, levels = levels(data$cells$sample)))

data$samples$by_cell_cycle_seurat <- data$cells %>%
  dplyr::group_by(sample, cell_cycle_seurat) %>%
  dplyr::summarize(count = dplyr::n()) %>%
  tidyr::spread(cell_cycle_seurat, count, fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
  dplyr::select(c('sample', 'total_cell_count', dplyr::everything())) %>%
  dplyr::arrange(factor(sample, levels = levels(data$cells$sample)))

data$samples$by_cell_cycle_cyclone <- data$samples$by_cell_cycle_seurat

data$clusters$by_samples <- data$cells %>%
  dplyr::group_by(cluster, sample) %>%
  dplyr::summarize(count = dplyr::n()) %>%
  tidyr::spread(sample, count, fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
  dplyr::select(c('cluster', 'total_cell_count', dplyr::everything())) %>%
  dplyr::arrange(factor(cluster, levels = levels(data$cells$cluster)))

data$clusters$by_cell_cycle_seurat <- data$cells %>%
  dplyr::group_by(cluster, cell_cycle_seurat) %>%
  dplyr::summarize(count = dplyr::n()) %>%
  tidyr::spread(cell_cycle_seurat, count, fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
  dplyr::select(c('cluster', 'total_cell_count', dplyr::everything())) %>%
  dplyr::arrange(factor(cluster, levels = levels(data$cells$cluster)))

data$clusters$by_cell_cycle_cyclone <- data$clusters$by_cell_cycle_seurat

data$most_expressed_genes$by_sample <- bind_rows(
  data$most_expressed_genes$by_sample %>% mutate(sample = 'pbmc_10k_v3_rep1'),
  data$most_expressed_genes$by_sample %>% mutate(sample = 'pbmc_10k_v3_rep2'),
  data$most_expressed_genes$by_sample %>% mutate(sample = 'pbmc_10k_v3_rep3')
) %>%
mutate(sample = factor(sample, levels = c('pbmc_10k_v3_rep1','pbmc_10k_v3_rep2','pbmc_10k_v3_rep3')))

cells <- sample(1:ncol(data$expression), 500)
genes <- sample(1:nrow(data$expression), 1000)

data$cells <- data$cells[cells,]
data$expression <- data$expression[genes,cells]
data$projections$tSNE <- data$projections$tSNE[cells,]
data$projections$tSNE_3D <- data$projections$tSNE_3D[cells,]
data$projections$UMAP <- data$projections$UMAP[cells,]
data$projections$UMAP_3D <- data$projections$UMAP_3D[cells,]
data$trajectory$monocle2$all_cells$meta <- data$trajectory$monocle2$all_cells$meta[cells,]
data$trajectory$monocle2$subset_of_cells$meta <- data$trajectory$monocle2$subset_of_cells$meta[cells,]

saveRDS(data, 'example.rds')
```
