# Ideas of what to implement in Cerebro/cerebroApp in the future

## General

- FAQ on website
- List articles that have used Cerebro.
- Update article about the Cerebro UI

## cerebroApp

- General
  - Use unit testing
  - Use modules in Shiny app where it makes sense
  - Warning when exporting very large expression data (e.g. non-sparse)
  - Check which tasks are carried out in multiple places and create function for them
  - Show warning if version of loaded Cerebro object doesn't match the current Cerebro UI version
  - Use glue to build messages.
  - Round values in DT column filtering for numeric values (currently it shows too many decimals)
  - Thorough performance check with profvis
  - Make functions compatible with SCE object. Shouldn’t be too much work.
  - Test memory usage with dgCMatrix and RleMatrix for larger data set.
  - Don’t call functions in CRB object that don’t exist.
  - “x_unified” for trend lines: hovering over trend line shows vertical line.
  - Horizontally align title in box and info button.
- New features
  - QC tab
  - Rename group levels, e.g. clusters
  - Assign new labels to cells based on selection (create new grouping variable on-the-fly)
  - Save changes to file
  - Visualize velocyto results (`ggquiver`)
  - Visualize PAGA results
  - Dot plot as alternative representation of expression of multiple genes across selected grouping variable
  - Perform statistical analysis on expression of selected genes
  - Allow export of multiple assays? Paves the way for scRNA-seq + scATAC-seq
  - Trajectory compatibility with slingshot and Monocle 3.
  - Upload gene list from file.
  - Drag resize scatter plots.
- Ideas
  - Use “usethis" package.
  - Function to update old Cerebro objects? It's easy to just export the Seurat/SCE object again to get the new format.
  - Store more data in `reactiveValues()`.
  - Clean up reactivity dependencies
  - Function to export cerebroApp for <shinyapps.io>
  - Use the [`bs4Dash` package](https://rinterface.github.io/bs4Dash/index.html)?
  - Display logo somewhere more visible in the app?
  - Build example data set into package and make it loadable.
  - Save colors in CRB object so it can be loaded next time. [would be solved with new dedicated classes]
  - Store description of extra material somewhere? [would be solved with new dedicated classes]
  - Dedicated classes for certain types of content
    - groups (also cell cycle), tables, plots, projections, trajectories
    - Allows storing descriptions along with tables/plots, etc.
    - The goal of having very specific classes for Cerebro is to have better control of the data format, as well as being able to combine different types of data that belong together, for example a table (data frame) and a description (string). While R is a functional programming language, I believe it is ok in the context of Cerebro to use introduce specific classes, because they are only meant to be used inside Cerebro, not for other purposes and not for further processing.
    - Possible problems of using specific classes:
      - How to deal with possible changes to the classes in the future? The object will look the same to Cerebro, but the fields/methods might be different.
    - Before implementing dedicated object classes, make sure they can be easily integrated in the next release.

## Standalone version

- Make it work
  - Figure out a way to programmatically create the standalone version, e.g. with the [`electricShine`](https://chasemc.github.io/electricShine/) package. Without that, I am stuck with the R version that was provided with the original framework, which is too old for some cerebroApp dependencies.
- Other
  - Test memory usage with dgCMatrix and RleMatrix for larger data set.

# Apps to take inspiration from

- [SCANNER](https://www.biorxiv.org/content/10.1101/2020.01.25.919712v2)
- [Spectacle](https://www.sciencedirect.com/science/article/pii/S0014483520304620)

# Other useful links

- Discussion on how to make caged/prisoned R installation: http://r.789695.n4.nabble.com/How-do-I-put-ldpaths-in-a-specific-place-td2964241.html
- Managing multiple R versions on one machine (could be useful for making standalone version)
  - https://mac.r-project.org/#nightly
  - http://r-sig-mac.29524.n8.nabble.com/R-SIG-Mac-R-latest-pkg-link-returning-a-403-Forbidden-error-td693.html#a702
  - https://rud.is/rswitch/guide/
  - https://jacobrprice.github.io/2019/09/19/Installing-multiple-parallel-R-versions.html
  - https://cran.rstudio.org/doc/manuals/R-admin.html#Uninstalling-under-macOS

# More details for some TO DOs

## Testing which genes are more variable than others in a given list of genes

```r
list.of.genes <- c('Abcc4','Abraxas2','Actn4','Acvr1','Adam9','Adamts5','Ager','Amer2','Ank2','Api5','Bach1','Baz2b','Bcl11a','Bcl2l2','Bcl9','Bmt2','1700025G04Rik','1190002N15Rik','Cacna1c','Cebpa','Chd4','Cit','Col23a1','Csk','Csnk1g3','Ctcf','Cul3','Dazl','Dbndd2','Dcun1d4','Ddx3x','Ddx3y','Dhx57','Dpp4','Dscam','Dtna','E2f3','Ehd1','Ephb1','Erc2','Etv3','Eya2','Fam214a','Gabarap','Galnt16','Gdf6','Git1','Gys1','Hdac4','Hnrnph3','Hspa13','Igfbp5','Katnbl1','Loxl3','Lrrc4','Lrrc8e','Map3k8','Mdga2','Mex3c','Mgat1','Mmd','Nkiras2','Nr3c1','Nsd3','Nutf2','Ogt','Ostm1','Pdgfra','Pfn1','Phf20l1','Phyhip','Pitx2','Ppp1cc','R3hdm1','Reep1','Rnf19a','Rtkn2','Senp1','Siah1a','Slc25a13','Slc38a2','Slc41a2','Slf2','Slmap','Snx2','Sox4','Srr','Stag1','Stradb','Syt6','Taf9b','Tbx3','Trp53inp2','Tshz1','Tspan2','Tssk2','Ttyh2','Vegfa','Wnt1','Yes1','Zbed4','Zbtb10','Zfp182','Zfp608','Zfp654')

group <-
table <- data.frame(gene=character(), p.value=numeric(), overall.mean=numeric(), overall.variance=numeric(), stringsAsFactors=FALSE)

data <- as.data.frame(t(as.matrix(sample_data$expression[ which(grepl(rownames(sample_data$expression), pattern=paste0('^', list.of.genes, '$', collapse='|'))),])))
data$group <- vapply(strsplit(rownames(data), '-'), `[`, 2, FUN.VALUE=character(1))

list.of.genes <- colnames(data)

lm(numbers ~ group, data)

for ( i in sample_data$samples$names ) {
  temp.table <- data.frame('.mean'=numeric(), '.variance'=numeric())
  colnames(temp.table) <- paste0(i, colnames(temp.table))
  table <- cbind(table, temp.table)
}

for ( i in 1:length(list.of.genes) ) {
  temp.numbers <- c()
  for ( j in sample_data$samples$names ) {
    temp.numbers <- c(temp.numbers, unlist(tapply(sample_data$expression[which(rownames(sample_data$expression) == list.of.genes[i]),], group, withinRunStats)[[j]]))
  }
  temp.data <- data.frame(group=group, numbers=sample_data$expression[which(rownames(sample_data$expression) == list.of.genes[i]),])
  table[i,] <- c(
    list.of.genes[i],
    anova(lm(numbers ~ group, temp.data))['group','Pr(>F)'],
    mean(sample_data$expression[which(rownames(sample_data$expression) == list.of.genes[i]),]),
    var(sample_data$expression[which(rownames(sample_data$expression) == list.of.genes[i]),]),
    temp.numbers
  )
}

table$p.value <- as.numeric(table$p.value)
```
