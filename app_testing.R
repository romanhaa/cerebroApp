library(dplyr)
library(HDF5Array)
library(shiny)
library(shinydashboard)

options(shiny.maxRequestSize = 2000 * 1024^2)

# if (!exists('crb')) {
#   crb <- readRDS('~/Downloads/sc_merge_cerebro_delayed.crb')
# }

Cerebro.options <- list(
  "mode" = "open",
  "cerebro_root" = "~/GitHub/cerebroApp_dev/inst",
  "crb_file_to_load" = "~/GitHub/cerebro_related_adventures/hdf5/sc_merge_cerebro_delayed_no_expression.crb",
  "expression_matrix_h5" = "~/GitHub/cerebro_related_adventures/hdf5/sc_merge_cerebro_delayed_expression_10x.h5",
  # "crb_file_to_load" = "~/GitHub/cerebro_related_adventures/sc_merge_cerebro_delayed.crb",
  "projections_default_point_size" = 2,
  "projections_default_point_opacity" = 1.0,
  "projections_default_percentage_cells_to_show" = 100,
  "projections_show_hover_info" = TRUE
)

source("~/GitHub/cerebroApp_dev/inst/shiny/v1.3/shiny_server.R")
source("~/GitHub/cerebroApp_dev/inst/shiny/v1.3/shiny_UI.R")

files_to_load <- list.files(
  "~/GitHub/cerebroApp_dev/R",
  pattern = ".R",
  full.names = TRUE
)
for ( i in files_to_load ) {
  source(i, local = TRUE)
}

mode_debugging <- 'v'

shiny::runApp(shiny::shinyApp(ui = ui, server = server))
