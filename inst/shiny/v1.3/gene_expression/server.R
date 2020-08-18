##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##----------------------------------------------------------------------------##

source(paste0(path_to_shiny_files, "/gene_expression/expression_projection.R"), local = TRUE)
source(paste0(path_to_shiny_files, "/gene_expression/expression_details_selected_cells.R"), local = TRUE)
source(paste0(path_to_shiny_files, "/gene_expression/expression_in_selected_cells.R"), local = TRUE)
source(paste0(path_to_shiny_files, "/gene_expression/expression_by_group.R"), local = TRUE)
source(paste0(path_to_shiny_files, "/gene_expression/expression_by_gene.R"), local = TRUE)

##----------------------------------------------------------------------------##
## Reactive data that holds genes provided by user or in selected gene set.
##----------------------------------------------------------------------------##

## cannot use req() because it delays initialization and plot is updated only
## with button press so plot doesn't initialize at all
genesToPlot <- reactive({

  ## prepare empty list for data
  genesToPlot <- list(
    "genes_to_display" = character(),
    "genes_to_display_present" = character(),
    "genes_to_display_missing" = character()
  )

  if ( input[["expression_analysis_mode"]] == "Gene(s)" ) {

    ## check if user provided input in gene box
    ## ... if user provided input
    if ( is.null(input[["expression_genes_input"]]) == FALSE ) {

      ## - grab user input
      ## - split by comma, space, semicolon and line
      ## - convert to vector
      ## - remove spaces
      ## - remove duplicated strings
      ## - remove empty strings
      genesToPlot[["genes_to_display"]] <- input[["expression_genes_input"]] %>%
        strsplit(",| |;|\n") %>%
        unlist() %>%
        gsub(pattern = " ", replacement = "", fixed = TRUE) %>%
        unique() %>%
        .[. != ""]
    }

  } else if ( input[["expression_analysis_mode"]] == "Gene set" ) {

    ##
    req(
      input[["expression_select_gene_set"]]
    )

    genesToPlot[["genes_to_display"]] <- getGenesForGeneSet(input[["expression_select_gene_set"]])
  }

  ## check which are available in the data set
  genes_to_display_here <- getGeneNames()[ match(tolower(genesToPlot[["genes_to_display"]]), tolower(getGeneNames())) ]

  ## get which genes are available in the data set
  genesToPlot[["genes_to_display_present"]] <- na.omit(genes_to_display_here)

  ## get names of provided genes that are not in the data set
  genesToPlot[["genes_to_display_missing"]] <- genesToPlot[["genes_to_display"]][ which(is.na(genes_to_display_here)) ]

  return(genesToPlot)
})

##----------------------------------------------------------------------------##
## Reactive data that holds data to be plotted.
##----------------------------------------------------------------------------##

gene_expression_plot_data <- reactive({

  ## don't proceed without these inputs
  req(
    input[["expression_projection_to_display"]],
    input[["expression_percentage_cells_to_show"]],
    input[["expression_projection_plotting_order"]],
    genesToPlot()
  )

  ## get names of all cells in data set
  cells_to_display <- getCellIDs()

  ## randomly remove cells (if necessary)
  percentage_cells_show <- input[["expression_percentage_cells_to_show"]]
  if ( percentage_cells_show < 100 ) {
    number_of_cells_to_plot <- ceiling(
      percentage_cells_show / 100 * length(cells_to_display)
    )
    cells_to_display <- cells_to_display[ sample(1:length(cells_to_display), number_of_cells_to_plot) ]
  }

  ## get position in projection and meta data for selected cells
  plot <- cbind(
      getProjection(input[["expression_projection_to_display"]])[ cells_to_display , ],
      getMetaData()[ cells_to_display , ]
    )

  ## get expression values that will be plotted; depends on how many genes are
  ## available
  ## ... no genes are available
  if ( length(genesToPlot()$genes_to_display_present) == 0 ) {

    ## set expression level to 0
    plot$level <- 0

  ## ... 1 gene is available
  } else if ( length(genesToPlot()$genes_to_display_present) == 1 ) {

    ## retrieve expression values for that gene
    plot$level <- getExpression()[
        genesToPlot()$genes_to_display_present ,
        cells_to_display
      ]

  ## ... at least 2 genes are available
  } else {

    ## calculate mean across all genes for each cell
    plot$level <- getExpression()[
        genesToPlot()$genes_to_display_present ,
        cells_to_display
      ] %>%
      Matrix::colMeans()
  }

  ## set plotting order, depending on user input
  plot_order <- input[["expression_projection_plotting_order"]]

  ## ... if plotting order is random
  if ( plot_order == "Random" ) {

    ## randomize row order
    plot <- plot[ sample(1:nrow(plot), nrow(plot)) , ]

  ## ... if plotting order is from high to low
  } else if ( plot_order == "Highest expression on top" ) {

    ## sort rows by expression level from low to high
    plot <- plot[ order(plot$level, decreasing = FALSE) , ]
  }

  return(plot)
})
