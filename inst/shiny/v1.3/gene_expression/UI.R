##----------------------------------------------------------------------------##
## Tab: Gene expression
##----------------------------------------------------------------------------##

tab_gene_expression <- tabItem(
  tabName = "geneExpression",
  fluidRow(
    column(width = 3, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = "Input parameters",
        tagList(
          uiOutput("expression_UI"),
          uiOutput("expression_color_scale_range"),
          uiOutput("expression_scales")
        )
      )
    ),
    column(width = 9, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = tagList(
          boxTitle("Dimensional reduction"),
          actionButton(
            inputId = "expression_projection_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 5px"
          ),
          shinySaveButton(
            "expression_projection_export",
            label = "export to PDF",
            title = "Export dimensional reduction to PDF file.",
            filetype = "pdf",
            viewtype = "icon",
            class = "btn-xs"
          )
        ),
        tagList(
          plotly::plotlyOutput(
            "expression_projection",
            width = "auto",
            height = "85vh"
          ),
          tags$br(),
          htmlOutput("expression_number_of_selected_cells"),
          tags$br(),
          htmlOutput("expression_genes_displayed")
        )
      )
    )
  ),
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Details of selected cells"),
        cerebroInfoButton("expression_details_selected_cells_info")
      ),
      tagList(
        shinyWidgets::materialSwitch(
          inputId = "expression_details_selected_cells_number_formatting",
          label = "Automatically format numbers:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "expression_details_selected_cells_color_highlighting",
          label = "Highlight values with colors:",
          value = FALSE,
          status = "primary",
          inline = TRUE
        ),
        DT::dataTableOutput("expression_details_selected_cells")
      )
    )
  ),
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels in selected cells"),
        cerebroInfoButton("expression_in_selected_cells_info")
      ),
      plotly::plotlyOutput("expression_in_selected_cells")
    )
  ),
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels by group"),
        cerebroInfoButton("expression_by_group_info")
      ),
      tagList(
        uiOutput("expression_by_group_selected_group_UI"),
        plotly::plotlyOutput("expression_by_group")
      )
    )
  ),

  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels by gene"),
        cerebroInfoButton("expression_by_gene_info")
      ),
      plotly::plotlyOutput("expression_by_gene")
    )
  )
)
