# Launching Cerebro with a loaded data set

```r
## load packages -------------------------------------------------------------##
library(dplyr)
library(DT)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

## load data set -------------------------------------------------------------##
my_data_set <<- readRDS("extdata/v1.3/example.crb")

## set parameters ------------------------------------------------------------##
Cerebro.options <<- list(
  "mode" = "closed",
  "crb_file_to_load" = "my_data_set",
  "cerebro_root" = "."
)

shiny_options <- list(
  maxRequestSize = 800 * 1024^2,
  port = 1337
)

## load server and UI functions ----------------------------------------------##
source(glue::glue("{Cerebro.options$cerebro_root}/shiny/v1.3/shiny_UI.R"))
source(glue::glue("{Cerebro.options$cerebro_root}/shiny/v1.3/shiny_server.R"))

## launch app ----------------------------------------------------------------##
shiny::shinyApp(
  ui = ui,
  server = server,
  options = shiny_options
)
```
