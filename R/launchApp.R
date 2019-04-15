#' Launch Cerebro app.
#' @export launchApp
#' @param maxFileSize Maximum size of input file; defaults to 800 MB.
#' @return Cerebro application object
#' @import dplyr
#' @importFrom formattable proportion
#' @import ggplot2
#' @importFrom ggtree geom_tree geom_tiplab geom_tippoint theme_tree
#' @import Matrix
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets

launchApp <- function(
  maxFileSize = 800
) {

  ##--------------------------------------------------------------------------##
  ##
  ##--------------------------------------------------------------------------##
  if ( grepl(tolower(Sys.info()["sysname"]), pattern = "^win") ) {
    .libPaths(paste0(getwd(), "/R-Portable-Win/library"))
    plot_export_path <- paste0(Sys.getenv("USERPROFILE"), "\\Desktop\\")
  } else if ( "DOCKER" %in% names(Sys.getenv()) ) {
    plot_export_path <- "/plots"
  } else if ( grepl(tolower(Sys.info()["sysname"]), pattern = "darwin") ) {
    .libPaths(paste0(getwd(), "/R-Portable-Mac/library"))
    plot_export_path <- "~/Desktop/"
  }

  ##--------------------------------------------------------------------------##
  ##
  ##--------------------------------------------------------------------------##
  source(system.file("shiny/overview/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/samples/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/clusters/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/most_expressed_genes/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/marker_genes/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/enriched_pathways/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/gene_expression/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/gene_set_expression/info.R", package = "cerebroApp"), local = TRUE)

  ##--------------------------------------------------------------------------##
  ## Allow upload of files up to 800 MB.
  ##--------------------------------------------------------------------------##
  options(shiny.maxRequestSize = maxFileSize * 1024^2) 

  ##--------------------------------------------------------------------------##
  ## Load server and UI functions.
  ##--------------------------------------------------------------------------##
  source(system.file("shiny/shiny_UI.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/shiny_server.R", package = "cerebroApp"), local = TRUE)

  ##--------------------------------------------------------------------------##
  ## Launch app.
  ##--------------------------------------------------------------------------##
  shinyApp(ui = ui, server = server)

}