##----------------------------------------------------------------------------##
## Layout of the UI elements.
##----------------------------------------------------------------------------##
output[["overview_projection_UI"]] <- renderUI({
  fluidRow(
    ## selections and parameters
    column(width = 3, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = tagList(
          "Main parameters",
          actionButton(
            inputId = "overview_projection_main_parameters_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-left: 5px"
          )
        ),
        uiOutput("overview_projection_main_parameters_UI")
      ),
      cerebroBox(
        title = tagList(
          "Additional parameters",
          actionButton(
            inputId = "overview_projection_additional_parameters_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-left: 5px"
          )
        ),
        uiOutput("overview_projection_additional_parameters_UI"),
        collapsed = TRUE
      ),
      cerebroBox(
        title = tagList(
          "Group filters",
          actionButton(
            inputId = "overview_projection_group_filters_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-left: 5px"
          )
        ),
        uiOutput("overview_projection_group_filters_UI"),
        collapsed = TRUE
      )
    ),
    ## plot
    column(width = 9, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = tagList(
          boxTitle("Dimensional reduction"),
          actionButton(
            inputId = "overview_projection_info",
            label = "info",
            title = "Show additional information for this panel.",
            icon = NULL,
            class = "btn-xs",
            style = "margin-right: 3px"
          ),
          shinyFiles::shinySaveButton(
            "overview_projection_export",
            label = "export to PDF",
            title = "Export dimensional reduction to PDF file.",
            filetype = "pdf",
            viewtype = "icon",
            class = "btn-xs",
            style = "margin-right: 3px"
          ),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("overview_projection_show_group_label_UI"),
              uiOutput("overview_projection_point_border_UI"),
              uiOutput("overview_projection_scales_UI")
            ),
            circle = FALSE,
            icon = icon("cog"),
            inline = TRUE,
            size = "xs"
          )
        ),
        tagList(
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              "overview_projection",
              width = "auto",
              height = "85vh"
            ),
            type = 8,
            hide.ui = FALSE
          ),
          tags$br(),
          htmlOutput("overview_number_of_selected_cells"),
        )
      )
    )
  )
})
