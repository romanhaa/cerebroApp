# How to generate the example GMT file

The `GMT` file contains 20 genes present in the example Seurat PBMC data set to ensure the `performGeneSetEnrichmentAnalysis()` function will work properly.
Storing gene sets in the `GMT` format requires a name, a description (here we used a URL), followed by the genes which belong to the set.
All elements are separated by tabs.
One gene set per line.

I have taken random genes from the Cerebro v1.3 example data set as shown below.

```r
pbmc <- read.table(file = system.file('extdata', 'pbmc_raw.txt', package = 'Seurat'), as.is = TRUE)
sample(rownames(pbmc), 20)
```
