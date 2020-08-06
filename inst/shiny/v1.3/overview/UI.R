##----------------------------------------------------------------------------##
## Tab: Overview
##----------------------------------------------------------------------------##

tab_overview <- tabItem(
  tabName = "overview",
  shinyjs::inlineCSS("
    #overview_details_selected_cells_table .table th {
      text-align: center;
    }
    #overview_details_selected_cells_table .dt-middle {
      vertical-align: middle;
    }
    "
  ),
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = "Input parameters",
        tagList(
          uiOutput("overview_UI"),
          uiOutput("overview_scales")
        )
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = tagList(
          boxTitle("Dimensional reduction"),
          actionButton(
            inputId = "overview_projection_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 5px"
          ),
          shinySaveButton(
            "overview_projection_export",
            label = "export to PDF",
            title = "Export dimensional reduction to PDF file.",
            filetype = "pdf",
            viewtype = "icon",
            class = "btn-xs"
          )
        ),
        tagList(
          plotly::plotlyOutput(
            "overview_projection",
            width = "auto",
            height = "85vh"
          ),
          tags$br(),
          htmlOutput("overview_number_of_selected_cells"),
        )
      )
    )
  ),
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Plot of selected cells"),
        cerebroInfoButton("overview_details_selected_cells_plot_info")
      ),
      plotly::plotlyOutput("overview_details_selected_cells_plot")
    )
  ),
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Table of selected cells"),
        cerebroInfoButton("overview_details_selected_cells_table_info")
      ),
      tagList(
        shinyWidgets::materialSwitch(
          inputId = "overview_details_selected_cells_table_number_formatting",
          label = "Automatically format numbers:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "overview_details_selected_cells_table_color_highlighting",
          label = "Highlight values with colors:",
          value = FALSE,
          status = "primary",
          inline = TRUE
        ),
        DT::dataTableOutput("overview_details_selected_cells_table")
      )
    )
  )
)
