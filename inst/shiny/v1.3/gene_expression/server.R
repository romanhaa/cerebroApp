##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##----------------------------------------------------------------------------##

source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_expression/projection.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_expression/table_of_selected_cells.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_expression/expression_in_selected_cells.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_expression/expression_by_group.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_expression/expression_by_gene.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_expression/expression_by_pseudotime.R"), local = TRUE)

##----------------------------------------------------------------------------##
## Reactive data that holds genes provided by user or in selected gene set.
##----------------------------------------------------------------------------##

## cannot use req() because it delays initialization and plot is updated only
## with button press so plot doesn't initialize at all
genesToPlot <- reactive({

  ## prepare empty list for data
  gene_sets <- list(
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
      gene_sets[["genes_to_display"]] <- input[["expression_genes_input"]] %>%
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

    gene_sets[["genes_to_display"]] <- getGenesForGeneSet(input[["expression_select_gene_set"]])
  }

  ## check which are available in the data set
  genes_to_display_here <- getGeneNames()[ match(tolower(gene_sets[["genes_to_display"]]), tolower(getGeneNames())) ]

  ## get which genes are available in the data set
  gene_sets[["genes_to_display_present"]] <- na.omit(genes_to_display_here)

  ## get names of provided genes that are not in the data set
  gene_sets[["genes_to_display_missing"]] <- gene_sets[["genes_to_display"]][ which(is.na(genes_to_display_here)) ]

  return(gene_sets)
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
    !is.null(input[["expression_projection_show_genes_in_separate_panels"]]),
    genesToPlot()
  )

  ## check if projection or trajectory should be shown
  ## ... projection
  if ( input[["expression_projection_to_display"]] %in% availableProjections() ) {

    ## build data frame with data
    cells_df <- cbind(
        getProjection(input[["expression_projection_to_display"]]),
        getMetaData()
      )

  ## ... trajectory
  } else {

    ## split selection into method and name
    selection <- strsplit(input[["expression_projection_to_display"]], split = ' // ')[[1]]

    ## check if method and name exist and don't proceed if not
    req(
      selection[1] %in% getMethodsForTrajectories(),
      selection[2] %in% getNamesOfTrajectories(selection[1])
    )

    ## collect trajectory data
    trajectory_data <- getTrajectory(
      selection[1],
      selection[2]
    )

    ## merge meta data and trajectory info and remove cells without pseudotime
    cells_df <- cbind(trajectory_data[["meta"]], getMetaData()) %>%
      dplyr::filter(!is.na(pseudotime))
  }

  ## randomly remove cells (if necessary)
  cells_df <- randomlySubsetCells(cells_df, input[["expression_percentage_cells_to_show"]])

  ## get expression values that will be plotted; depends on how many genes are
  ## available
  ## ... no genes are available
  if ( length(genesToPlot()$genes_to_display_present) == 0 ) {

    ## set expression level to 0
    cells_df$level <- 0

  ## ... at least 1 gene is available
  } else {

    ## check if user requested to show expression in separate panels
    ## ... separate panels requested, at least 2 genes but not more than 8
    ##     genes selected
    if (
      input[["expression_projection_show_genes_in_separate_panels"]] == TRUE &&
      input[["expression_projection_to_display"]] %in% availableProjections() &&
      length(genesToPlot()$genes_to_display_present) >= 2 &&
      length(genesToPlot()$genes_to_display_present) <= 8
    ) {

      ## - get expression matrix
      ## - transpose matrix
      ## - convert to data frame with genes as columns and cells as rows
      ## - add projection coordinates (only first two columns because 3D is not
      ##   supported anyway)
      ## - bring data in longer format
      ## NOTE: I don't merge the expression value with cell meta data because
      ##       hover info doesn't work properly anyway so like this the data
      ##       frame stays smaller, especially with large data sets
      cells_df <- getExpressionMatrix(
          cells = cells_df$cell_barcode,
          genes = genesToPlot()$genes_to_display_present
        ) %>%
        Matrix::t() %>%
        as.data.frame() %>%
        cbind(cells_df[,1:2], .) %>%
        tidyr::pivot_longer(
          cols = tidyselect::all_of(genesToPlot()$genes_to_display_present),
          names_to = "gene",
          values_to = "level"
        )

    ## ... if proper conditions for separate panels are not met
    } else {

      ## calculate mean across all genes for each cell
      cells_df$level <- getMeanExpressionForCells(
        cells = cells_df$cell_barcode,
        genes = genesToPlot()$genes_to_display_present
      )
    }
  }

  ## set plotting order, depending on user input
  plot_order <- input[["expression_projection_plotting_order"]]

  ## ... if plotting order is random
  if ( plot_order == "Random" ) {

    ## randomize row order
    cells_df <- cells_df[ sample(1:nrow(cells_df), nrow(cells_df)) , ]

  ## ... if plotting order is from high to low
  } else if ( plot_order == "Highest expression on top" ) {

    ## sort rows by expression level from low to high
    cells_df <- cells_df[ order(cells_df$level, decreasing = FALSE) , ]
  }

  return(cells_df)
})
