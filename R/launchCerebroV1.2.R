#' Launch Cerebro v1.2
#' @title Launch Cerebro v1.2
#' @description Launch the Cerebro v1.2 Shiny application.
#' @keywords Cerebro scRNAseq Seurat
#' @param maxFileSize Maximum size of input file; defaults to 800 MB.
#' @export
#' @return Shiny application.
#' @importFrom colourpicker colourInput
#' @import dplyr
#' @importFrom DT datatable formatStyle
#' @importFrom formattable proportion
#' @import ggplot2
#' @importFrom ggtree geom_tree geom_tiplab geom_tippoint theme_tree
#' @importFrom msigdbr msigdbr
#' @importFrom plotly layout plot_ly plotlyOutput renderPlotly toWebGL
#' @importFrom reshape2 melt
#' @importFrom tidyr spread
#' @import scales
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @importFrom viridis scale_fill_viridis
#' @examples
#' if ( interactive() ) {
#'   launchCerebroV1.2(maxFileSize = 800)
#' }
launchCerebroV1.2 <- function(
  maxFileSize = 800
){

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
  shiny::shinyApp(ui = ui, server = server)

}
