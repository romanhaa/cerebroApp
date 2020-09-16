#' @title
#' Launch Cerebro v1.2
#'
#' @description
#' Launch the Cerebro v1.2 Shiny application.
#'
#' @param maxFileSize Maximum size of input file; defaults to \code{800}
#' (800 MB).
#' @param ... Further parameters that are used by \code{shiny::runApp}, e.g.
#' \code{host} or \code{port}.
#'
#' @return
#' Shiny application.
#'
#' @examples
#' if ( interactive() ) {
#'   launchCerebroV1.2(maxFileSize = 800)
#' }
#'
#' @importFrom colourpicker colourInput
#' @import dplyr
#' @importFrom DT datatable formatStyle
#' @import ggplot2
#' @importFrom msigdbr msigdbr
#' @importFrom plotly layout plot_ly plotlyOutput renderPlotly toWebGL
#' @importFrom tidyr spread
#' @import scales
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @importFrom viridis scale_fill_viridis
#'
#' @export
#'
launchCerebroV1.2 <- function(
  maxFileSize = 800,
  ...
){

  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  ## check if ggtree package is installed
  if ( !requireNamespace("ggtree", quietly = TRUE) ) {
    stop(
      "The 'ggtree' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## check if formattable package is installed
  if ( !requireNamespace("formattable", quietly = TRUE) ) {
    stop(
      "The 'formattable' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## check if reshape2 package is installed
  if ( !requireNamespace("reshape2", quietly = TRUE) ) {
    stop(
      "The 'reshape2' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ##--------------------------------------------------------------------------##
  ## define path to export plots to
  ##--------------------------------------------------------------------------##
  if ( grepl(tolower(Sys.info()["sysname"]), pattern = "^win") )
  {
    plot_export_path <- paste0(Sys.getenv("USERPROFILE"), "\\Desktop\\")
  } else if ( "DOCKER" %in% names(Sys.getenv()) )
  {
    plot_export_path <- "/plots"
  } else if ( grepl(tolower(Sys.info()["sysname"]), pattern = "darwin") )
  {
    plot_export_path <- "~/Desktop/"
  }

  ##--------------------------------------------------------------------------##
  ## Allow upload of files up to 800 MB.
  ##--------------------------------------------------------------------------##
  options(shiny.maxRequestSize = maxFileSize * 1024^2)

  ##--------------------------------------------------------------------------##
  ## Load server and UI functions.
  ##--------------------------------------------------------------------------##
  source(
    system.file(
      paste0("shiny/v1.2/shiny_UI.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("shiny/v1.2/shiny_server.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## Launch app.
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '##---------------------------------------------------------------------------##\n',
      '## Launching Cerebro v1.2\n',
      '##---------------------------------------------------------------------------##'
    )
  )
  shiny::shinyApp(ui = ui, server = server, ...)
}
