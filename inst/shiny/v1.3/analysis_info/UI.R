##----------------------------------------------------------------------------##
## Tab: Analysis info
##----------------------------------------------------------------------------##

tab_analysis_info <- tabItem(
  tabName = "analysis_info",
  fluidRow(
    column(12,
      titlePanel("Information about the data set and analysis"),
      htmlOutput("data_set_info_general")
    )
  )
)
