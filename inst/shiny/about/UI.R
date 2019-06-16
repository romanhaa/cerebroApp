##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

tab_about <- tabItem(
  tabName = "about",
  fluidPage(
    fluidRow(
      column(12,
        titlePanel("About this application"),
        htmlOutput("about"),
        uiOutput("webgl_switch_and_indicator"),
        actionButton("browser", "browser"),
        tags$script("$('#browser').hide();")
      )
    )
  )
)
