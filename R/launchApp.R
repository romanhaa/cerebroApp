#' Launch Cerebro app.
#' @export launchApp
#' @return shiny application object
#' @import dplyr
#' @import DT
#' @import formattable
#' @import ggplot2
#' @import ggtree
#' @import Matrix
#' @import msigdbr
#' @import plotly
#' @import RColorBrewer
#' @import reshape2
#' @import scales
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets

launchApp <- function() {

  ##----------------------------------------------------------------------------##
  ##
  ##----------------------------------------------------------------------------##
  if (grepl(tolower(Sys.info()['sysname']), pattern='^win')) {
    .libPaths(paste0(getwd(), "/R-Portable-Win/library"))
    plot_export_path <- paste0(Sys.getenv("USERPROFILE"), "\\Desktop\\")
  } else {
    .libPaths(paste0(getwd(), "/R-Portable-Mac/library"))
    plot_export_path <- "~/Desktop/"
  }

  ##----------------------------------------------------------------------------##
  ##
  ##----------------------------------------------------------------------------##
  source(system.file("shiny/overview/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/samples/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/clusters/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/most_expressed_genes/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/marker_genes/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/enriched_pathways/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/gene_expression/info.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/gene_set_expression/info.R", package = "cerebroApp"), local = TRUE)

  ##----------------------------------------------------------------------------##
  ## Allow upload of files up to 800 MB.
  ##----------------------------------------------------------------------------##
  options(shiny.maxRequestSize = 800*1024^2) 

  ##----------------------------------------------------------------------------##
  ## Load server and UI functions.
  ##----------------------------------------------------------------------------##
  source(system.file("shiny/shiny_UI.R", package = "cerebroApp"), local = TRUE)
  source(system.file("shiny/shiny_server.R", package = "cerebroApp"), local = TRUE)

  ##----------------------------------------------------------------------------##
  ## Launch app.
  ##----------------------------------------------------------------------------##
  shinyApp(ui = ui, server = server)

}