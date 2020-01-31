#' Launch Cerebro interface.
#' @title Launch Cerebro interface.
#' @description Launch Cerebro interface.
#' @keywords Cerebro scRNAseq Seurat
#' @param version Which version of Cerebro to launch: "v1.0", "v1.1", "v1.2".
#' Defaults to "v1.2".
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
  version = "v1.2",
  maxFileSize = 800
){
  ##--------------------------------------------------------------------------##
  ##
  ##--------------------------------------------------------------------------##
  available_versions = c("v1.0","v1.1","v1.2")
  if ( (version %in% available_versions == FALSE ) )
  {
    stop(
      'Version must be one of: ', paste0(available_versions, collapse = ', '),
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
  ##
  ##--------------------------------------------------------------------------##
  source(
    system.file(
      paste0("shiny/", version, "/overview/info.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("shiny/", version, "/samples/info.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("shiny/", version, "/clusters/info.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("shiny/", version, "/most_expressed_genes/info.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("shiny/", version, "/marker_genes/info.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("shiny/", version, "/enriched_pathways/info.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("shiny/", version, "/gene_expression/info.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("shiny/", version, "/gene_set_expression/info.R"),
      package = "cerebroApp"
    ),
    local = TRUE
  )
  if ( version %in% c('v1.1','v1.2') ) {
    source(
      system.file(
        paste0("shiny/", version, "/trajectory/info.R"),
        package = "cerebroApp"
      ),
      local = TRUE
    )
    source(
      system.file(
        paste0("shiny/", version, "/color_management/info.R"),
        package = "cerebroApp"
      ),
      local = TRUE
    )
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
  shiny::shinyApp(ui = ui, server = server)

}
