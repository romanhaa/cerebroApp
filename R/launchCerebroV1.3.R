#' @title
#' Launch Cerebro v1.3
#'
#' @description
#' Launch the Cerebro v1.3 Shiny application.
#'
#' @param mode Cerebro can be ran in \code{open} or \code{closed} mode, allowing
#' the user to load their own data set (\code{open}) or only show a pre-loaded
#' data set (\code{closed}, removes the "Load data" element); defaults to
#' \code{open}.
#' @param maxFileSize Maximum size of input file; defaults to \code{800}
#' (800 MB).
#' @param crb_file_to_load Path to \code{.crb} file to load on launch of
#' Cerebro. Useful when using/hosting Cerebro in \code{closed} mode. Defaults to
#' \code{NULL}.
#' @param welcome_message \code{string} with custom welcome message to display
#' in the "Load data" tab. Can contain HTML formatting, e.g.
#' \code{'<h3>Hi!</h3>'}. Defaults to \code{NULL}.
#' @param ... Further parameters that are used by \code{shiny::runApp}, e.g.
#' \code{host} or \code{port}.
#'
#' @return
#' Shiny application.
#'
#' @examples
#' if ( interactive() ) {
#'   launchCerebroV1.3(
#'     mode = "open",
#'     maxFileSize = 800
#'   )
#' }
#'
#' @importFrom ape plot.phylo
#' @importFrom colourpicker colourInput
#' @import dplyr
#' @importFrom DT datatable formatPercentage formatRound formatSignif
#' formatStyle styleColorBar styleEqual styleInterval
#' @import ggplot2
#' @importFrom grDevices col2rgb rgb
#' @importFrom msigdbr msigdbr
#' @importFrom plotly add_lines add_trace event_data layout plot_ly plotlyOutput
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
#' @importFrom viridis scale_fill_viridis
#'
#' @export
#'
launchCerebroV1.3 <- function(
  mode = "open",
  maxFileSize = 800,
  crb_file_to_load = NULL,
  welcome_message = NULL,
  ...
){

  ##--------------------------------------------------------------------------##
  ## Check validity of 'mode' parameter.
  ##--------------------------------------------------------------------------##
  if ( mode %in% c('open','closed') == FALSE ) {
    stop(
      "'mode' parameter must be set to either 'open' or 'closed'.",
      call. = FALSE
    )
  }

  ##--------------------------------------------------------------------------##
  ## Create global variable with options that need to be available inside the
  ## Shiny app.
  ##--------------------------------------------------------------------------##
  Cerebro.options <<- list(
    "mode" = mode,
    "crb_file_to_load" = crb_file_to_load,
    "welcome_message" = welcome_message,
    "cerebro_root" = system.file(package = "cerebroApp")
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
    ...
  )
}
