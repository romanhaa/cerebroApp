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

      ## print log message
      print(glue::glue("[{Sys.time()}] Loaded data set: {input[['input_file']]$datapath}"))

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

      ## print log message
      print(glue::glue("[{Sys.time()}] Loaded data set: {.GlobalEnv$Cerebro.options[['crb_file_to_load']]}"))

    ## ... no file was specified to be loaded and no file has been selected in
    ##     the "Load data" UI element
    } else {

      ## load small example data set
      data <- readRDS(
        system.file("extdata/v1.3/example.crb", package = "cerebroApp")
      )

      ## print log message
      print(glue::glue("[{Sys.time()}] Loaded example data set..."))

    }

    message(data$print())

    ## check if 'expression' slot exists and print log message with its format
    ## if it does
    if ( !is.null(data$expression) ) {
      print(glue::glue("[{Sys.time()}] Format of expression data: {class(data$expression)}"))
    }

    ## return loaded data
    return(data)
  })

  ##--------------------------------------------------------------------------##
  ## Show "Trajectory" tab if there are trajectories in the data set.
  ##--------------------------------------------------------------------------##

  ## the tab item needs to be in the `output`
  output[["sidebar_item_trajectory"]] <- renderMenu({
    menuItem("Trajectory", tabName = "trajectory", icon = icon("random"))
  })

  ## this reactive value checks whether the tab should be shown or not
  show_trajectory_tab <- reactive({

    ## require a data set to be loaded
    req(
      !is.null(data_set())
    )

    ## if at least one trajectory is present, return TRUE, otherwise FALSE
    if (
      !is.null(getMethodsForTrajectories()) &&
      length(getMethodsForTrajectories()) > 0
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  ## listen to reactive value defined above and toggle visibility of trajectory
  ## tab accordingly
  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_trajectory",
      condition = show_trajectory_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Show "Extra material" tab if there is some extra material in the data set.
  ##--------------------------------------------------------------------------##

  ## the tab item needs to be in the `output`
  output[["sidebar_item_extra_material"]] <- renderMenu({
    menuItem("Extra material", tabName = "extra_material", icon = icon("gift"))
  })

  ## this reactive value checks whether the tab should be shown or not
  show_extra_material_tab <- reactive({

    ## require a data set to be loaded
    req(
      !is.null(data_set())
    )

    ## if at least one piece of extra material is present, return TRUE,
    ## otherwise FALSE
    if (
      !is.null(getExtraMaterialCategories()) &&
      length(getExtraMaterialCategories()) > 0
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  ## listen to reactive value defined above and toggle visibility of extra
  ## material tab accordingly
  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_extra_material",
      condition = show_extra_material_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Print log message when switching tab (for debugging).
  ##--------------------------------------------------------------------------##
  observe({
    print(glue::glue("[{Sys.time()}] Active tab: {input[['sidebar']]}"))
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
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/trajectory/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/extra_material/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_id_conversion/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/analysis_info/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/color_management/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/about/server.R"), local = TRUE)
}
