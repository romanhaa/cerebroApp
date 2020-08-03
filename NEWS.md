# cerebroApp 1.3.0

## Major changes

- With data sets becoming more complex, users often have more than just the two grouping variables Cerebro was initially made to work with ('sample' and 'cluster'). To provide a more generalized interface, users can now specify a list of grouping variables. Consequently, the 'Samples' and 'Clusters' tabs in the Cerebro interface have been replaced with a single 'Groups' interface, where users can select one of the available grouping variables (with the same content as before). This can be useful when you cluster the cells with different methods/settings and want to provide the Cerebro user with both results.
- Removed support for Seurat objects before v3.0. Users who need to continue working with older version of Seurat have two options: (1) use the `Seurat::UpdateSeuratObject()` function to update their Seurat object before exporting it for visualization in Cerebro; (2) use older Cerebro version. I apologize for the any trouble this may cause.
- Data loaded into Cerebro is now stored as a `SingleCellExperiment` (SCE) object. This streamlines the internal interaction with the data and ensures a stable structure for future releases.
- Due to the changes in data structure, files exported with cerebroApp v1.3 can only be visualized in Cerebro v1.3. Moreover, files exported with cerebroApp v1.2 and earlier cannot be loaded into Cerebro v1.3. Again, I apologize for any inconvenience but I believe these changes will lead to more stability coming releases.

## New features

- It is now possible to export single cell data stored in `SingleCellExperiment` (SCE) objects.

## Minor changes

- Hierarchical trees can be useful to show similarities between cell populations. Previously, a tree could only be visualized for clusters. By modifying the cell identities in a Seurat object, the Seurat function `BuildClusterTree()` can be used to calculate trees also for other cell groupings. To allow exporting multiple trees, they need to now be stored as a `list()` in the `@misc` or the `@metadata` slot of Seurat or SingleCellExperiment objects, respectively.

# cerebroApp 1.2.1

## New features

- It is now possible to select cells in the dimensional reduction plots ('Overview', 'Gene expression', and 'Gene set expression' tabs) and retrieve additional info for them. For example, users can get tables of meta data or expression values and save them as a file for further analysis. Also, gene expression can be shown in the selected vs. non-selected cells.

## Minor changes

- Scales for expression levels by sample and cluster in "Gene expression" and "Gene set expression" tabs are now set to be from 0 to 1.2 times the highest value. This is to limit the violin plots which cannot be trimmed to the actual data range and will extend beyond, giving a false impression of negative values existing in the data.
- Hover info in expression by gene plot in "Gene expression" and "Gene set expression" tabs now show both the gene name and the mean expression value instead of just the gene name.

# cerebroApp 1.2.0

## New features

- New button for composition plots (e.g. samples by clusters or cell cycle) that allows to choose whether to scale by actual cell count (default) or percentage.
- New button for composition plots that allows to show/hide the respective table of numbers behind them.
- New tab "Color management": Users can now change the color assigned to each sample/cluster.
- "Gene expression" and "Gene set expression" panels: Users can now pick from a set of color scales and adjust the color range.
- The gene selection box in the "Gene expression" panel will now allow to view available genes and select them by clicking. It is not necessary anymore to hit Enter or Space to update the plot, this will be done automatically after providing new input.
- It is now possible to export assays other than `RNA` through the `assay` parameter in relevant functions.
- Launch old Cerebro interfaces through `version` parameter in `launchCerebro()`.
- We added a vignette which explains how to use cerebroApp and its functions.

## Minor changes

- Add citation info.
- Composition tables (e.g. samples by clusters or cell cycle) are now calculated in the Shiny app rather than being expected to be present in the `.crb file.
- Fix log message in `exportFromSeurat()` when extracting trajectories.
- The gene set selection box in the "Gene set expression" tab will not crash anymore when typing a sequence of letters that doesn't match any gene set names.
- Remove dependency on pre-assigned colors in the `.crb` file. If no colors have been assigned to samples and clusters when loading a data set, they will be assigned then.
- Update examples of functions and include mini-Seurat object and example gene set (GMT file) to run the examples.
- Modify pre-loaded data set in Cerebro interface to contain more data.
- When attempting to download genes in GO term "cell surface" in the `getMarkerGenes()` function, it tries at max. 3 times to contact the biomaRt server and continues without if all attempts failed. Sometimes the server does not respond which gave an error in previous versions of the function.
- Plenty of changes to meet Bioconductor guidelines (character count per line, replace `.` in dplyr pipes with `rlang::.data`, etc.).
- Reduce package size by compressing reference files, e.g. gene name/ID conversion tables.

# cerebroApp 1.1.0

- Release along with manuscript revision.

## New features

- New function `extractMonocleTrajectory()`: Users can extract data from trajectories calculated with Monocle v2.
- New tab "Trajectories": Allows visualization of trajectories calculated with Monocle v2.

# cerebroApp 1.0.0

- Public release along with manuscript submission to bioRxiv.
