# Ideas of what to implement in Cerebro/cerebroApp in the future

Many of these are just wild ideas, some are pretty unlikely to be implemented but I wanted to keep track of the ideas anyway.

* Store cerebroApp as subfolder in Cerebro repository.
* Use SingleCellExperiment as an object to store data. This would make conversion easier and the structure more intuitive.
* Extract trajectories from slingshot. Detect what type of trajectory it is and then plot accordingly.
* Test genes (single and gene sets) for the ones which are highly expressed and which are different between the samples/clusters (idea is ANOVA but it's slow).
* Make function to extract trajectory from Monocle 3 and display in Cerebro.
* Use “usethis" package.
* Bring back gene id conversion.
* Include report of read counts.
* Upload own list of genes.
* Add progress indicators.
* Use semantic.dashboard
* Check if difference of expression between samples is significant or not.
* Come up with sustainable update strategy.
* Make Linux version.
* Allow users to change colors in plots.
* Send log file through email.
* Horizontally align title in box and info button.
* Drag resize scatter plots.
* Find a way to display optional info fields.
* Mark mitochondrial and ribosomal genes in tables.
* Gene set names is cut off when too long, make box larger or resizable?
* Allow users to label/rename clusters.
* Save changes to input file.
* pyqt to make it faster?
* Python Dash + Python Qt + fbs??
    * Can this be bottled up?

# Other useful links

* Discussion on how to make caged/prisoned R installation: http://r.789695.n4.nabble.com/How-do-I-put-ldpaths-in-a-specific-place-td2964241.html
* Generating Rd files for documentation: https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html

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
