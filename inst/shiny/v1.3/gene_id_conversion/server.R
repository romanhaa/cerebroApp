##----------------------------------------------------------------------------##
## Table of gene IDs and symbols.
##----------------------------------------------------------------------------##
output[["gene_info"]] <- DT::renderDataTable({
  if ( input[["geneIdConversion_organism"]] == "mouse" ) {
    conversion_table <- read.table(
      paste0(Cerebro.options$cerebro_root, "/extdata/mm10_gene_ID_name.tsv.gz"),
      sep = "\t", header = TRUE, stringsAsFactors = FALSE
    )
  } else if ( input[["geneIdConversion_organism"]] == "human" ) {
    conversion_table <- read.table(
      paste0(Cerebro.options$cerebro_root, "/extdata/hg38_gene_ID_name.tsv.gz"),
      sep = "\t", header = TRUE, stringsAsFactors = FALSE
    )
  }
  DT::datatable(
    conversion_table,
    filter = "none",
    selection = "multiple",
    escape = FALSE,
    autoHideNavigation = TRUE,
    rownames = FALSE,
    options = list(
      scrollX = FALSE,
      dom = "Bfrtip",
      lengthMenu = c(15, 30, 50, 100),
      pageLength = 50
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["geneIdConversion_info"]], {
  showModal(
    modalDialog(
      geneIdConversion_info[["text"]],
      title = geneIdConversion_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
geneIdConversion_info <- list(
  title = "Gene ID/symbol conversion",
  text = p("Conversion table containing Gencode identifiers, Ensembl identifiers, Havana identifiers, gene symbol and gene type for mouse (version M16) and human (version 27).")
)
