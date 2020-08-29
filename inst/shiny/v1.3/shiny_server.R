##----------------------------------------------------------------------------##
## Server function for Cerebro.
##----------------------------------------------------------------------------##
server <- function(input, output, session) {

  ##--------------------------------------------------------------------------##
  ## Load color setup, plotting and utility functions.
  ##--------------------------------------------------------------------------##
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/color_setup.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/plotting_functions.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/utility_functions.R"), local = TRUE)

  ##--------------------------------------------------------------------------##
  ## Central parameters.
  ##--------------------------------------------------------------------------##
  scatter_plot_point_size <- list(
    min = 1,
    max = 20,
    step = 1,
    default = 5
  )

  scatter_plot_point_opacity <- list(
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

  available_storage_volumes <- c(
    Home = "~",
    shinyFiles::getVolumes()()
  )

  ##--------------------------------------------------------------------------##
  ## Load data set.
  ##--------------------------------------------------------------------------##
  data_set <- reactive({

    ## check what data to load
    ## ... file was selected in the "Load data" UI element and it also
    if (
      !is.null(input[["input_file"]]) &&
      !is.na(input[["input_file"]]) &&
      file.exists(input[["input_file"]]$datapath)
    ) {

      ## load specified file
      data <- readRDS(input[["input_file"]]$datapath)

    ## ... a .crb file was specified to be loaded into Cerebro on launch, the
    ##     file exists, and no selection has been made through the "Load data"
    ##     UI element
    } else if (
      exists('Cerebro.options') &&
      !is.null(.GlobalEnv$Cerebro.options[["crb_file_to_load"]]) &&
      file.exists(.GlobalEnv$Cerebro.options[["crb_file_to_load"]])
    ) {

      ## load the specified file
      data <- readRDS(.GlobalEnv$Cerebro.options[["crb_file_to_load"]])

    ## ... no file was specified to be loaded and no file has been selected in
    ##     the "Load data" UI element
    } else {

      ## load small example data set
      data <- readRDS(
        system.file("extdata/v1.3/example.crb", package = "cerebroApp")
      )
    }

    ## return loaded data
    return(data)
  })

  ##--------------------------------------------------------------------------##
  ## Sidebar menu.
  ##--------------------------------------------------------------------------##
  output[["sidebar_menu"]] <- renderMenu({
    sidebarMenu(id = "sidebar",
      menuItem("Load data", tabName = "loadData", icon = icon("spinner"), selected = TRUE),
      menuItem("Overview", tabName = "overview", icon = icon("binoculars")),
      menuItem("Groups", tabName = "groups", icon = icon("star")),
      menuItem("Most expressed genes", tabName = "mostExpressedGenes", icon = icon("bullhorn")),
      menuItem("Marker genes", tabName = "markerGenes", icon = icon("magnet")),
      menuItem("Enriched pathways", tabName = "enrichedPathways", icon = icon("sitemap")),
      menuItem("Gene (set) expression", tabName = "geneExpression", icon = icon("signal")),
      menuItem("Trajectory", tabName = "trajectory", icon = icon("random")),
      menuItem("Gene ID conversion", tabName = "geneIdConversion", icon = icon("barcode")),
      menuItem("Analysis info", tabName = "info", icon = icon("info")),
      menuItem("Color management", tabName = "color_management", icon = icon("palette")),
      menuItem("About", tabName = "about", icon = icon("at"))
    )
  })

  ##--------------------------------------------------------------------------##
  ## Tabs.
  ##--------------------------------------------------------------------------##
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/load_data/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/overview/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/groups/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/most_expressed_genes/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/marker_genes/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/enriched_pathways/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_expression/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_id_conversion/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/trajectory/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/analysis_info/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/color_management/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/about/server.R"), local = TRUE)
}
