[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Lifecycle: retired](https://img.shields.io/badge/lifecycle-retired-red.svg)
[![Twitter](https://img.shields.io/twitter/url/https/twitter.com/fakechek1.svg?label=Follow%20%40fakechek1&style=social)](https://twitter.com/fakechek1)

<img align="right" width="35%" height="auto" src="vignettes/logo_Cerebro.png">

:warning: __Discontinuation notice:__ Sadly, [Cerebro](https://github.com/romanhaa/Cerebro) and [cerebroApp](https://github.com/romanhaa/cerebroApp) are no longer in active development. See [here](./DISCONTINUATION_NOTICE.md) for more info.

# cerebroApp

R package upon which the [Cerebro](https://github.com/romanhaa/Cerebro) is built.
Contains helper function that prepare single-cell RNA-seq data stored in a Seurat object for visualization in Cerebro.
Seurat v3 and `SCE`/`SingleCellExperiment` objects are supported.

Make sure to install the package using `BiocManager::install()` because `devtools::install_github()` will otherwise pull old versions of dependencies that can result in errors.

```r
BiocManager::install('romanhaa/cerebroApp')
```

For further details, please refer to the official [cerebroApp](https://romanhaa.github.io/cerebroApp/) website.

## Credit

* Pathway enrichment in marker gene lists (`getEnrichedPathways()`) is done through the enrichR API (<https://github.com/wjawaid/enrichR>). I took the `enrichr` function and modified it to run in parallel (`future_lapply`) and not print status messages.
* Gene set enrichment analysis (`performGeneSetEnrichmentAnalysis()`) is performed using the [GSVA](https://bioconductor.org/packages/release/bioc/html/GSVA.html) R package. p- and q-value statistics are calculated through the same method as used by "Evaluation of methods to assign cell type labels to cell clusters from single-cell RNA-sequencing data", Diaz-Mejia *et al*., F1000Research (2019). [Link to publication](https://f1000research.com/articles/8-296/v2)
