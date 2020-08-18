#' Launch Cerebro interface.
#' @title Launch Cerebro interface.
#' @description Launch Cerebro interface.
#' @keywords Cerebro scRNAseq Seurat
#' @param version Which version of Cerebro to launch: "v1.0", "v1.1", "v1.2",
#' "v1.3"; defaults to "v1.3".
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
#'   launchCerebro(maxFileSize = 800)
#' }
launchCerebro <- function(
  version = "v1.3",
  mode = "open",
  maxFileSize = 800
){
  ##--------------------------------------------------------------------------##
  ##
  ##--------------------------------------------------------------------------##
  available_versions <- c("v1.0","v1.1","v1.2","v1.3")
  if ( (version %in% available_versions == FALSE ) )
  {
    stop(
      'Version must be one of: ', paste0(available_versions, collapse = ', '),
      call. = FALSE
    )
  }

  ##--------------------------------------------------------------------------##
  ##
  ##--------------------------------------------------------------------------##
  Cerebro.options <<- list(
    "mode" = mode
  )

  ##--------------------------------------------------------------------------##
  ## define path to export plots to
  ## only necessary in old Cerebro version because a dialog is opened to choose
  ## file destination since v1.3
  ##--------------------------------------------------------------------------##
  if ( version %in% c("v1.0","v1.1","v1.2") ) {
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
      paste0("shiny/", version, "/shiny_UI.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("shiny/", version, "/shiny_server.R"),
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
      '## Launching Cerebro ', version, '\n',
      '##---------------------------------------------------------------------------##'
    )
  )
  shiny::shinyApp(ui = ui, server = server)

}
