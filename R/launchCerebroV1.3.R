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
#' @param projections_default_point_size Default point size in projections. This
#' value can be changed in the UI; defaults to 5.
#' @param projections_default_point_opacity Default point opacity in
#' projections. This value can be changed in the UI; defaults to 1.0.
#' @param projections_default_percentage_cells_to_show Default percentage of
#' cells to show in projections. This value can be changed in the UI; defaults
#' to 100.
#' @param projections_show_hover_info Show hover infos in projections. This
#" setting can be changed in the UI; defaults to TRUE.
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
  projections_default_point_size = 5,
  projections_default_point_opacity = 1.0,
  projections_default_percentage_cells_to_show = 100,
  projections_show_hover_info = TRUE,
  ...
){

  ##--------------------------------------------------------------------------##
  ## Check validity of input parameters.
  ##--------------------------------------------------------------------------##
  if ( mode %in% c('open','closed') == FALSE ) {
    stop(
      "'mode' parameter must be set to either 'open' or 'closed'.",
      call. = FALSE
    )
  }
  if (
    projections_default_point_size < 0 ||
    projections_default_point_size > 20
  ) {
    stop(
      "'projections_default_point_size' parameter must be between 1 and 20",
      call. = FALSE
    )
  }
  if (
    projections_default_point_opacity < 0 ||
    projections_default_point_opacity > 1
  ) {
    stop(
      "'projections_default_point_opacity' parameter must be between 0 and 1",
      call. = FALSE
    )
  }
  if (
    projections_default_percentage_cells_to_show < 0 ||
    projections_default_percentage_cells_to_show > 100
  ) {
    stop(
      "'projections_default_percentage_cells_to_show' parameter must be between 0 and 100",
      call. = FALSE
    )
  }
  if ( projections_show_hover_info %in% c(TRUE, FALSE) == FALSE ) {
    stop(
      "'projections_show_hover_info' parameter must be set to either TRUE or FALSE.",
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
    "cerebro_root" = system.file(package = "cerebroApp"),
    "projections_default_point_size" = projections_default_point_size,
    "projections_default_point_opacity" = projections_default_point_opacity,
    "projections_default_percentage_cells_to_show" = projections_default_percentage_cells_to_show,
    "projections_show_hover_info" = projections_show_hover_info
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
