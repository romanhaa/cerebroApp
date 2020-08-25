#' Launch Cerebro v1.3
#' @title Launch Cerebro v1.3
#' @description Launch the Cerebro v1.3 Shiny application.
#' @keywords Cerebro scRNAseq Seurat
#' @param mode Cerebro can be ran in "open" or "closed" mode, allowing the user
#' to load their own data set ("open") or only show a pre-loaded data set
#' ("closed", removes the "Load data" element); defaults to "open".
#' @param maxFileSize Maximum size of input file; defaults to 800 MB.
#' @param crb_file_to_load Path to .crb file to load on launch of Cerebro.
#' Useful when using/hosting Cerebro in "closed" mode. Defaults to NULL.
#' @param options ...
#' @export
#' @return Shiny application.
#' @importFrom ape plot.phylo
#' @importFrom colourpicker colourInput
#' @import dplyr
#' @importFrom DT datatable formatPercentage formatRound formatSignif
#' formatStyle styleColorBar styleEqual styleInterval
#' @import ggplot2
#' @importFrom grDevices col2rgb rgb
#' @importFrom msigdbr msigdbr
#' @importFrom plotly add_trace event_data layout plot_ly plotlyOutput
#' renderPlotly toWebGL
#' @importFrom stringr str_length
#' @importFrom tidyr pivot_longer pivot_wider
#' @import scales
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @import shinydashboard
#' @importFrom shinyFiles getVolumes parseSavePath shinyFileSave shinySaveButton
#' @importFrom shinyjs inlineCSS
#' @importFrom shinyWidgets awesomeCheckbox dropdownButton materialSwitch
#' radioGroupButtons sendSweetAlert
#' @import SingleCellExperiment
#' @importFrom viridis scale_fill_viridis
#' @examples
#' if ( interactive() ) {
#'   launchCerebroV1.3(
#'     mode = "open",
#'     maxFileSize = 800
#'   )
#' }
launchCerebroV1.3 <- function(
  mode = "open",
  maxFileSize = 800,
  crb_file_to_load = NULL,
  options = list()
){

  ## TODO: figure out how to make this unnecessary; does the data set have to be
  ##       exported again?
  require("SingleCellExperiment")

  ##--------------------------------------------------------------------------##
  ## Create global variable with options that need to be available inside the
  ## Shiny app.
  ##--------------------------------------------------------------------------##
  Cerebro.options <<- list(
    "mode" = mode,
    "crb_file_to_load" = crb_file_to_load,
    "path_to_shiny_files" = system.file("shiny","v1.3", package = "cerebroApp")
  )

  ##--------------------------------------------------------------------------##
  ## Allow upload of files up to 800 MB.
  ##--------------------------------------------------------------------------##
  options(shiny.maxRequestSize = maxFileSize * 1024^2)

  ##--------------------------------------------------------------------------##
  ## Load server and UI functions.
  ##--------------------------------------------------------------------------##
  source(
    system.file(
      paste0("shiny/v1.3/shiny_UI.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("shiny/v1.3/shiny_server.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## Launch Cerebro.
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '##---------------------------------------------------------------------------##\n',
      '## Launching Cerebro v1.3\n',
      '##---------------------------------------------------------------------------##'
    )
  )
  shiny::shinyApp(
    ui = ui,
    server = server,
    options = options
  )

}
