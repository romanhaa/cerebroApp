##----------------------------------------------------------------------------##
##
##----------------------------------------------------------------------------##
server <- function(input, output, session) {

  ##--------------------------------------------------------------------------##
  ## Color management.
  ##--------------------------------------------------------------------------##
  # Dutch palette from flatuicolors.com
  colors_dutch <- c(
    "#FFC312","#C4E538","#12CBC4","#FDA7DF","#ED4C67",
    "#F79F1F","#A3CB38","#1289A7","#D980FA","#B53471",
    "#EE5A24","#009432","#0652DD","#9980FA","#833471",
    "#EA2027","#006266","#1B1464","#5758BB","#6F1E51"
  )

  # Spanish palette from flatuicolors.com
  colors_spanish <- c(
    "#40407a","#706fd3","#f7f1e3","#34ace0","#33d9b2",
    "#2c2c54","#474787","#aaa69d","#227093","#218c74",
    "#ff5252","#ff793f","#d1ccc0","#ffb142","#ffda79",
    "#b33939","#cd6133","#84817a","#cc8e35","#ccae62"
  )

  colors <- c(colors_dutch, colors_spanish)

  cell_cycle_colorset <- setNames(
    c("#45aaf2", "#f1c40f", "#e74c3c", "#7f8c8d"),
    c("G1",      "S",       "G2M",     "-")
  )

  ##--------------------------------------------------------------------------##
  ## Central parameters.
  ##--------------------------------------------------------------------------##
  scatter_plot_dot_size <- list(
    min = 1,
    max = 20,
    step = 1,
    default = 5
  )

  scatter_plot_dot_opacity <- list(
    min = 0.1,
    max = 1.0,
    step = 0.1,
    default = 1.0
  )

  scatter_plot_percentage_cells_to_show <- list(
    min = 10,
    max = 100,
    step = 10,
    default = 100
  )

  preferences <- reactiveValues(use_webgl = TRUE)

  ##--------------------------------------------------------------------------##
  ## Sidebar menu.
  ##--------------------------------------------------------------------------##
  output[["sidebar_menu"]] <- renderMenu({
    sidebarMenu(id = "sidebar",
      menuItem(
        "Load data", tabName = "loadData",
        icon = icon("spinner"), selected = TRUE
      ),
      menuItem(
        "Overview", tabName = "overview",
        icon = icon("binoculars")
      ),
      menuItem(
        "Samples", tabName = "samples",
        icon = icon("star")
      ),
      menuItem(
        "Clusters", tabName = "clusters",
        icon = icon("braille")
      ),
      menuItem(
        "Most expressed genes", tabName = "mostExpressedGenes",
        icon = icon("bullhorn")
      ),
      menuItem(
        "Marker genes", tabName = "markerGenes",
        icon = icon("magnet")
      ),
      menuItem(
        "Enriched pathways", tabName = "enrichedPathways",
        icon = icon("sitemap")
      ),
      menuItem(
        "Gene expression", tabName = "geneExpression",
        icon = icon("signal")
      ),
      menuItem(
        "Gene set expression", tabName = "geneSetExpression",
        icon = icon("list")
      ),
      menuItem(
        "Trajectory", tabName = "trajectory",
        icon = icon("random")
      ),
      menuItem(
        "Gene ID conversion", tabName = "geneIdConversion",
        icon = icon("barcode")
      ),
      menuItem(
        "Analysis info", tabName = "info",
        icon = icon("info")
      ),
      menuItem(
        "About", tabName = "about",
        icon = icon("at")
      )
    )
  })

  ##--------------------------------------------------------------------------##
  ## Sample data.
  ##--------------------------------------------------------------------------##
  sample_data <- reactive({
    if ( is.null(input[["input_file"]]) || is.na(input[["input_file"]]) ) {
      sample_data <- readRDS(system.file("resources/example.rds", package = "cerebroApp"))
    } else {
      req(input[["input_file"]])
      sample_data <- readRDS(input[["input_file"]]$datapath)
    }
    sample_data$sample_names <- levels(sample_data$cells$sample)
    sample_data$cluster_names <- levels(sample_data$cells$cluster)
    sample_data
  })

  ##--------------------------------------------------------------------------##
  ## Function to calculate A-by-B tables (e.g. samples by clusters).
  ##--------------------------------------------------------------------------##
  calculateTableAB <- function(groupA, groupB) {
    # check if specified group columns exist in meta data
    if ( groupA %in% colnames(sample_data()$cells) == FALSE ) {
      stop(paste0("Column specified as groupA (`", groupA, "`) could not be found in meta data."), call. = FALSE)
    }
    if ( groupB %in% colnames(sample_data()$cells) == FALSE ) {
      stop(paste0("Column specified as groupB (`", groupB, "`) could not be found in meta data."), call. = FALSE)
    }
    table <- sample_data()$cells[,c(groupA,groupB)]
    # factorize group columns A and B if not already a factor
    if ( is.character(table[,groupA]) ) {
      table[[,groupA]] <- factor(
        table[[,groupA]],
        levels = table[[,groupA]] %>% unique() %>% sort()
      )
    }
    if ( is.character(table[,groupB]) ) {
      table[[,groupB]] <- factor(
        table[[,groupB]],
        levels = table[[,groupB]] %>% unique() %>% sort()
      )
    }
    # generate table
    table <- table %>%
      dplyr::group_by_at(c(groupA,groupB)) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      tidyr::spread(`groupB`, count, fill = 0) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
      dplyr::select(c(`groupA`, 'total_cell_count', dplyr::everything()))
    # fix order of columns if cell cycle info was chosen as second group
    if ( 'G1' %in% colnames(table) && 'G2M' %in% colnames(table) && 'S' %in% colnames(table) ) {
      table <- table %>%
        dplyr::select(c(`groupA`, 'total_cell_count', 'G1', 'S', 'G2M', dplyr::everything()))
    }
    # return
    return(table)
  }

  ##--------------------------------------------------------------------------##
  ## Tabs.
  ##--------------------------------------------------------------------------##
  source(system.file("shiny/load_data/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/overview/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/samples/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/clusters/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/most_expressed_genes/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/marker_genes/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/enriched_pathways/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/gene_expression/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/gene_set_expression/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/gene_id_conversion/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/trajectory/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/analysis_info/server.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/about/server.R", package = "cerebroApp"), local = TRUE)

}