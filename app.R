library(dplyr)
library(shiny)
library(shinydashboard)

options(shiny.maxRequestSize = 800 * 1024^2)

Cerebro.options <<- list(
  "mode" = "open",
  "cerebro_root" = "~/GitHub/cerebroApp_dev_refactor_overview/inst"
)

source("~/GitHub/cerebroApp_dev_refactor_overview/inst/shiny/v1.3/shiny_server.R")
source("~/GitHub/cerebroApp_dev_refactor_overview/inst/shiny/v1.3/shiny_UI.R")

shiny::shinyApp(ui = ui, server = server)
