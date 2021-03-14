##----------------------------------------------------------------------------##
## Reactive data that holds genes provided by user or in selected gene set.
##----------------------------------------------------------------------------##
## cannot use req() because it delays initialization and plot is updated only
## with button press so plot doesn't initialize at all
expression_selected_genes <- reactive({
  req(
    input[["expression_analysis_mode"]],
    list_of_genes()
  )
  # message('--> trigger "expression_selected_genes"')
  ## prepare empty list for data
  gene_sets <- list(
    "genes_to_display" = character(),
    "genes_to_display_present" = character(),
    "genes_to_display_missing" = character()
  )
  ## ...
  if ( input[["expression_analysis_mode"]] == "Gene(s)" ) {
    ## check if user provided input in gene box
    ## ... if user provided input
    if ( !is.null(input[["expression_genes_input"]]) ) {
      ## - grab user input
      ## - split by comma, space, semicolon and line
      ## - convert to vector
      ## - remove spaces
      ## - remove duplicated strings
      ## - remove empty strings
      gene_sets[["genes_to_display"]] <- input[["expression_genes_input"]] %>%
        strsplit(",| |;|\n") %>%
        unlist() %>%
        gsub(pattern = " ", replacement = "", fixed = TRUE) %>%
        unique() %>%
        .[. != ""]
    }
  ## ...
  } else if ( input[["expression_analysis_mode"]] == "Gene set" ) {
    req(input[["expression_select_gene_set"]])
    gene_sets[["genes_to_display"]] <- getGenesForGeneSet(input[["expression_select_gene_set"]])
  }
  ## check which are available in the data set
  genes_to_display_here <- list_of_genes()[ match(tolower(gene_sets[["genes_to_display"]]), tolower(list_of_genes())) ]
  ## get which genes are available in the data set
  gene_sets[["genes_to_display_present"]] <- na.omit(genes_to_display_here)
  ## get names of provided genes that are not in the data set
  gene_sets[["genes_to_display_missing"]] <- gene_sets[["genes_to_display"]][ which(is.na(genes_to_display_here)) ]
  # message(str(gene_sets))
  return(gene_sets)
})
