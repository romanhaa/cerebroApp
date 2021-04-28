#' @title test
#' @description test
#' @param gene_set test
#' @param species test
#' @return test
.getGenesForGeneSet <- function(gene_set, species) {
  if (!is.null(species) && species == "mm") {
    species <- "Mus musculus"
  } else if (!is.null(species) && species == "hg") {
    species <- "Homo sapiens"
  } else {
    species <- "Mus musculus"
  }
  ## - get list of gene set names
  ## - filter for selected gene set
  ## - extract genes that belong to the gene set
  ## - get orthologs for the genes
  ## - convert gene symbols to vector
  ## - only keep unique gene symbols
  ## - sort genes
  msigdbr:::msigdbr_genesets[,1:2] %>%
  dplyr::filter(.data$gs_name == gene_set) %>%
  dplyr::inner_join(
    .,
    msigdbr:::msigdbr_genes,
    by = "gs_id"
  ) %>%
  dplyr::inner_join(
    .,
    msigdbr:::msigdbr_orthologs %>%
      dplyr::filter(.data$species_name == species) %>%
      dplyr::select(human_entrez_gene, gene_symbol),
    by = "human_entrez_gene"
  ) %>%
  dplyr::pull(gene_symbol) %>%
  unique() %>%
  sort()
}
