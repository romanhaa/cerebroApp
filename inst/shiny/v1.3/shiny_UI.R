##----------------------------------------------------------------------------##
## Custom functions.
##----------------------------------------------------------------------------##
cerebroBox <- function(title, content) {
  box(
    title = title,
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    content
  )
}

cerebroInfoButton <- function(id) {
  actionButton(
    inputId = id,
    label = "info",
    icon = NULL,
    class = "btn-xs",
    title = "Show additional information for this panel."
  )
}

boxTitle <- function(title) {
  p(title, style = "padding-right: 5px; display: inline")
}

##----------------------------------------------------------------------------##
## Load UI content for each tab.
##----------------------------------------------------------------------------##
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/load_data/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/overview/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/groups/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/most_expressed_genes/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/marker_genes/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/enriched_pathways/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_expression/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/trajectory/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/extra_material/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_id_conversion/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/analysis_info/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/color_management/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/about/UI.R"), local = TRUE)

##----------------------------------------------------------------------------##
## Create dashboard with different tabs.
##----------------------------------------------------------------------------##
ui <- dashboardPage(
  title = "Cerebro",
  dashboardHeader(
    title = span("Cerebro ", style = "color: white; font-size: 28px; font-weight: bold")
  ),
  dashboardSidebar(
    tags$head(tags$style(HTML(".content-wrapper {overflow-x: scroll;}"))),
    sidebarMenu(
      id = "sidebar",
      menuItem("Load data", tabName = "loadData", icon = icon("spinner"), selected = TRUE),
      menuItem("Overview", tabName = "overview", icon = icon("binoculars")),
      menuItem("Groups", tabName = "groups", icon = icon("star")),
      menuItem("Most expressed genes", tabName = "mostExpressedGenes", icon = icon("bullhorn")),
      menuItem("Marker genes", tabName = "markerGenes", icon = icon("magnet")),
      menuItem("Enriched pathways", tabName = "enrichedPathways", icon = icon("sitemap")),
      menuItem("Gene (set) expression", tabName = "geneExpression", icon = icon("signal")),
      menuItemOutput("sidebar_item_trajectory"),
      menuItemOutput("sidebar_item_extra_material"),
      menuItem("Gene ID conversion", tabName = "geneIdConversion", icon = icon("barcode")),
      menuItem("Analysis info", tabName = "analysis_info", icon = icon("info")),
      menuItem("Color management", tabName = "color_management", icon = icon("palette")),
      menuItem("About", tabName = "about", icon = icon("at"))
    )
  ),
  dashboardBody(
    tags$script(HTML('$("body").addClass("fixed");')),
    tabItems(
      tab_load_data,
      tab_overview,
      tab_groups,
      tab_most_expressed_genes,
      tab_marker_genes,
      tab_enriched_pathways,
      tab_gene_expression,
      tab_trajectory,
      tab_extra_material,
      tab_gene_id_conversion,
      tab_analysis_info,
      tab_color_management,
      tab_about
    )
  )
)

