# cerebroApp 1.3.1

Despite the minor version bump, this update contains substantial performance improvements in the Shiny app, specifically in the projections.

## Major changes

- Projections in the "Overview" and "Gene (set) expression" are now updated using the `Plotly.react()` Javascript function instead of redrawn from scratch inside R when changing the input variables. For the user, that means that (1) plots are drawn much quicker and (2) the current zoom/pan settings are maintained when switching plot parameters (coloring variable, point size/opacity, etc).

## Minor changes

- It is now possible to define several settings related to the projections shown in the "Overview" and "Gene (set) expression" tabs. For example, you can change the default point size and opacity, the default percentage of cells to show, and whether or not hover info should be activated in the projections. These settings are optional but useful when hosting a known data sets in `closed` mode, e.g. because you want to decrease the point size in a large data set. The respective parameters can be found in the description of the `launchCerebroV1.3()` function.
- Hover/tooltip info for cells in projections can be deactivated through a checkbox on the "About" tab. Deactivating hover info increases performance of projections.
- Hover/tooltip info for cells in the gene expression projection no longer contain the gene expression value. This is because preparing the hover info is an expensive computation with little return. As a result of removing the gene expression value, the hover info does not need to be recalculated every time a gene is added to or removed from the list of genes to show expression for. For the same reason, when plotting a trajectory, the state and pseudotime are not added to the hover info either.
- Internally, data for plotting in projections is rearranged, stored in different variables, and the final output is debounced to avoid unnecessary redrawing of the projections on initialization.
- The feature to show expression of multiple genes in separate panels has been matured. Up to 9 genes can be shown in a 3x3 panel matrix but all share the same color scale. While cells can be selected in any of the panels, the expression levels shown in the other UI element, e.g. table of selected cells or expression by group, refers to the mean expression of all selected genes (not just the one the cells were selected in).
- When coloring cells in projections by a caterogical variable, e.g. cell type, the dots in the legend are now larger and independent from the selected point size.
- Tables are now rendered server-side to improve performance for large tables.
- Cellular barcodes in tables of selected cells are formatted in monospace font.
- Columns in meta data tables, e.g. table of cells selected in projections, which are identified to contain percentage on a 0-100 scale are changed to a 0-1 scale to prevent non-sensical values such as 500%.
- Add comma to Y axis and hover info in bar chart of selected cells in projection ("Overview" tab).
- The `crb_file_to_load` parameter of the `launchCerebroV1.3()` function (or as part of `Cerebro.options`) can now be set to the name of a `Cerebro_v1.3` object. That means you can load the data set before launching Cerebro (with `readRDS()`) and make Cerebro initialize itself with it. This is particularly useful when hosting Cerebro in `closed` mode, preventing that each user session has to read the data set from disk.
- Update author info in "About" tab.

## Fixes

- Colors assigned to groups in bar chart of selected cells in projection ("Overview" tab) sometimes did not match those shown in the projection. This only applied to categorical grouping variables that are not registered as grouping variables.
- Update Enrichr API for `getEnrichedPathways()` function. Make it configurable in case of further changes to the API.

# cerebroApp 1.3.0

Because this is a relatively big release, I have prepared a dedicated article with release notes for cerebroApp v1.3 that you can find in the navigation bar.

## Major changes

- With data sets becoming more complex, users often have more than just the two grouping variables Cerebro was initially made to work with ('sample' and 'cluster'). To provide a more generalized interface, users can now specify multiple grouping variables (or a single one). Consequently, the 'Samples' and 'Clusters' tabs in the Cerebro interface have been replaced by the 'Groups' tab, where users can select one of the available grouping variables (with the same content as before). This can be useful when you cluster the cells with different methods/settings or have additional grouping variables, such as treatments, and want to provide the Cerebro user with both results.
- Data loaded into Cerebro is now stored in a dedicated class: `Cerebro_v1.3`.
- Due to the changes in data structure, files exported with cerebroApp v1.3 can only be visualized in Cerebro v1.3. Moreover, files exported with cerebroApp v1.2 and earlier cannot be loaded into Cerebro v1.3. I apologize for any inconvenience but I believe these changes will lead to more stability coming releases.
- Removed support for Seurat objects before v3.0. Users who need to continue working with older version of Seurat have two options: (1) use the `Seurat::UpdateSeuratObject()` function to update their Seurat object before exporting it for visualization in Cerebro; (2) use older Cerebro version. I apologize for the any trouble this may cause.
- The "Gene expression" and "Gene set expression" tabs have been merged into the new "Gene (set) expression)" which gives you access to both.
- The new "Extra material" tab allows you to export additional material related to the data set that you want to share with others. At the moment, only tables and plots (from ggplot2) are supported, but support for other types of content can be added upon user request in the future.

## New features

- It is now possible to export single cell data stored in `SingleCellExperiment` (SCE) objects.
- Gene (set) expression can now also be visualized in trajectories (generated by Monocle 2).
- `NA` values for cell assignment to one of the specified grouping variables will be replaced by `N/A` and put into a separate group ("N/A") when exporting the data.

# cerebroApp 1.2.2

## Fixes

- The title in the browser tab now correctly says "Cerebro" instead of containing some HTML code.
- Cluster trees should now be displayed correctly.
- `getEnrichedPathways()` no longer results in an error when marker genes are present but no database returns any enriched pathways, e.g. because there are too few marker genes. Thanks to @turkeyri for pointing it out and suggesting a solution!

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
- Composition tables (e.g. samples by clusters or cell cycle) are now calculated in the Shiny app rather than being expected to be present in the `.crb` file.
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
