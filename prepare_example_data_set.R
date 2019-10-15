
data <- readRDS('cerebro_pbmc_10k_v3_2019-10-10.crb')

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
