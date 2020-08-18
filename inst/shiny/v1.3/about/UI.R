##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

tab_about <- tabItem(
  tabName = "about",
  tagList(
    fluidRow(
      column(12,
        titlePanel("About Cerebro")
      ),
      column(8,
        htmlOutput("about"),
        uiOutput("preferences"),
        actionButton("browser", "browser"),
        tags$script("$('#browser').hide();")
      ),
      column(4,
        imageOutput("logo_Cerebro")
      )
    ),
    fluidRow(
      htmlOutput("about_footer")
    )
  )
)
