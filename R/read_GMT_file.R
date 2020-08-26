#' Read GMT file.
#' @title Read GMT file.
#' @description This functions reads a (tab-delimited) GMT file which contains
#' the gene set name in the first column, the gene set description in the
#' second column, and the gene names in the following columns.
#' @param file Path to GMT file.
#' @return Returns an object in the same format as from the GSA.read.gmt
#' function (GSA package) with gene sets, gene set names, and gene set
#' descriptions stored in lists.
#' @import dplyr
.read_GMT_file <- function(
  file
) {

  ## read provided file
  gmt <- readr::read_delim(
    file,
    delim = ';',
    col_names = c('X1'),
    col_types = readr::cols()
  )

  ## prepare empty list for results
  gene_set_genes <- list()

  ## cycle through gene sets (rows)
  for ( i in seq_len(nrow(gmt)) ) {

    ## split line content by tab
    temp_genes <- strsplit(gmt$X1[i], split = '\t')[[1]] %>% unlist()

    ## extract genes from line content
    temp_genes <- temp_genes[3:length(temp_genes)]

    ## save gene names
    gene_set_genes[[i]] <- temp_genes
  }

  ## create list of gene sets
  gene_set_loaded <- list(
    genesets = gene_set_genes,
    geneset.names = lapply(strsplit(gmt$X1, split = '\t'), '[', 1) %>% unlist(),
    geneset.description = lapply(
        strsplit(gmt$X1, split = '\t'), '[', 2
      ) %>% unlist()
  )

  ##
  return(gene_set_loaded)
}
