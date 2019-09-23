#' Read GMT file.
#' @title Read GMT file.
#' @description This functions reads a (tab-delimited) GMT file which contains
#' the gene set name in the first column, the gene set description in the
#' second column, and the gene names in the following columns.
#' @param file Path to GMT file.
#' @return Returns an object in the same format as from the GSA.read.gmt
#' function (GSA package) with gene sets, gene set names, and gene set
#' descriptions stored in lists.
#' @keywords seurat cerebro
#' @import dplyr
#' @examples
#' my_gmt <- read_GMT_file(
#'   file = 'path/to/my/gene_sets.gmt'
#' )

read_GMT_file <- function(file)
{
  gmt <- readr::read_delim(
    file,
    delim = ';',
    col_names = c('X1'),
    col_types = readr::cols()
  )

  gene_set_genes <- list()
  for ( i in 1:nrow(gmt) )
  {
    temp_genes <- strsplit(gmt$X1[i], split = '\t')[[1]] %>% unlist()
    temp_genes <- temp_genes[3:length(temp_genes)]
    gene_set_genes[[i]] <- temp_genes
  }
  gene_set_loaded <- list(
    genesets = gene_set_genes,
    geneset.names = lapply(strsplit(gmt$X1, split = '\t'), '[', 1) %>% unlist(),
    geneset.description = lapply(strsplit(gmt$X1, split = '\t'), '[', 2) %>% unlist()
  )

  return(gene_set_loaded)
}
