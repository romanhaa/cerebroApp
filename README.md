[![](https://img.shields.io/github/license/romanhaa/cerebroApp)](LICENSE.md)
[![](https://img.shields.io/twitter/url/https/twitter.com/fakechek1.svg?label=Follow%20%40fakechek1&style=social)](https://twitter.com/fakechek1)

# cerebroApp

R package upon which the [Cerebro](https://github.com/romanhaa/Cerebro) is built.
Contains helper function that prepare single-cell RNA-seq data stored in a Seurat object for visualization in Cerebro.
Both Seurat v2 and Seurat v3 are supported.

Make sure to install the package using `BiocManager::install()` because `devtools::install_github()` will otherwise pull old versions of dependencies that can result in errors.

```r
BiocManager::install('romanhaa/cerebroApp')
```

For further details, please refer to the official [Cerebro](https://github.com/romanhaa/Cerebro) repository.

## How to use

### Export data stored in Seurat object

**Required meta data:**

* Experiment name.
* Organism, e.g. 'hg' (human) or 'mm' (mouse).
* Sample.
* Cluster.
* Number of transcripts (usually created by Seurat by default; `nUMI` / `nCounts_RNA` in Seurat v2 and v3).
* Number of expressed genes (usually created by Seurat by default; `nGene` / `nFeature_RNA` in Seurat v2 and v3).

**Note:** It is recommended to save sample information in a column called `sample` and cluster information in a column called `cluster`. Otherwise, the respective column names need to specified below.

Prepare data:

```r
library('cerebroApp')
exportFromSeurat(seurat, 'my_experiment.crb')
```

### Launch Cerebro

Launch Cerebro and load the RDS file you just exported from R.

```r
launchCerebro()
```

### Optional (but recommended) steps

To take full advantage of Cerebro, it is recommended to also run the commands below before exporting the data.

```r
seurat <- addPercentMtRibo(seurat)
seurat <- getMostExpressedGenes(seurat)
seurat <- getMarkerGenes(seurat)
seurat <- getEnrichedPathways(seurat)
seurat <- performGeneSetEnrichmentAnalysis(seurat, GMT_file = 'c2.all.v7.0.symbols.gmt')
```

## Credit

* Pathway enrichment in marker gene lists (`getEnrichedPathways()`) is done through the enrichR API (<https://github.com/wjawaid/enrichR>). I took the `enrichr` function and modified it to run in parallel (`future_lapply`) and not print status messages.
* Gene set enrichment analysis (`performGeneSetEnrichmentAnalysis()`) is performed using the [GSVA](https://bioconductor.org/packages/release/bioc/html/GSVA.html) R package. p- and q-value statistics are calculated through the same method as used by "Evaluation of methods to assign cell type labels to cell clusters from single-cell RNA-sequencing data", Diaz-Mejia *et al*., F1000Research (2019). [Link to publication](https://f1000research.com/articles/8-296/v2)
