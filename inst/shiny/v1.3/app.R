version <- "v1.3"

##--------------------------------------------------------------------------##
##
##--------------------------------------------------------------------------##
## TODO: change path
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/overview/info.R", local = TRUE)
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/groups/info.R", local = TRUE)
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/most_expressed_genes/info.R", local = TRUE)
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/marker_genes/info.R", local = TRUE)
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/enriched_pathways/info.R", local = TRUE)
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/gene_expression/info.R", local = TRUE)
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/gene_set_expression/info.R", local = TRUE)
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/trajectory/info.R", local = TRUE)
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/color_management/info.R", local = TRUE)

##--------------------------------------------------------------------------##
## Allow upload of files up to 800 MB.
##--------------------------------------------------------------------------##
options(shiny.maxRequestSize = 200 * 1024^2)

##--------------------------------------------------------------------------##
## Load server and UI functions.
##--------------------------------------------------------------------------##
## TODO: change path
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/shiny_UI.R")
source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/shiny_server.R")

##--------------------------------------------------------------------------##
## Launch app.
##--------------------------------------------------------------------------##
shiny::shinyApp(ui = ui, server = server)
