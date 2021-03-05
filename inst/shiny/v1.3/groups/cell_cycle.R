##----------------------------------------------------------------------------##
## Composition by cell cycle.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["groups_cell_cycle_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Cell cycle assignments"),
        cerebroInfoButton("groups_by_cell_cycle_info")
      ),
      tagList(
        uiOutput("groups_by_cell_cycle_UI_buttons"),
        uiOutput("groups_by_cell_cycle_UI_rest")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows buttons or a text message if data is not
## available.
##----------------------------------------------------------------------------##
output[["groups_by_cell_cycle_UI_buttons"]] <- renderUI({
  if ( length(getCellCycle()) > 0 ) {
    tagList(
      selectInput(
        "groups_by_cell_cycle_column",
        label = "Column to take data from:",
        choices = getCellCycle()
      ),
      fluidRow(
        column(
          width = 3,
          shinyWidgets::radioGroupButtons(
             inputId = "groups_by_cell_cycle_plot_type",
             label = NULL,
             choices = c("Bar chart", "Sankey plot"),
             status = "primary",
             justified = TRUE,
             width = "100%",
             size = "sm"
          )
        ),
        column(
          width = 9,
          style = "padding: 5px;",
          shinyWidgets::materialSwitch(
            inputId = "groups_by_cell_cycle_show_as_percent",
            label = "Show composition as percent [%]:",
            status = "primary",
            inline = TRUE
          ),
          shinyWidgets::materialSwitch(
            inputId = "groups_by_cell_cycle_show_table",
            label = "Show table:",
            status = "primary",
            inline = TRUE
          )
        )
      )
    )
  } else {
    textOutput("groups_by_cell_cycle_text")
  }
})

##----------------------------------------------------------------------------##
## UI element that either shows the plot (and a table if selected) or nothing.
##----------------------------------------------------------------------------##
output[["groups_by_cell_cycle_UI_rest"]] <- renderUI({
  if ( length(getCellCycle()) > 0 ) {
    tagList(
      plotly::plotlyOutput("groups_by_cell_cycle_plot"),
      {
        if ( !is.null(input[["groups_by_cell_cycle_show_table"]]) && input[["groups_by_cell_cycle_show_table"]] == TRUE ) {
          DT::dataTableOutput("groups_by_cell_cycle_table")
        }
      }
    )
  }
})

##----------------------------------------------------------------------------##
## Bar plot.
##----------------------------------------------------------------------------##
output[["groups_by_cell_cycle_plot"]] <- plotly::renderPlotly({
  req(
    input[["groups_selected_group"]] %in% getGroups(),
    input[["groups_by_cell_cycle_column"]] %in% getCellCycle(),
    input[["groups_by_cell_cycle_plot_type"]]
  )
  if ( input[["groups_by_cell_cycle_plot_type"]] == "Bar chart" ) {
    ## calculate table
    composition_df <- calculateTableAB(
      getMetaData(),
      input[[ "groups_selected_group" ]],
      input[[ "groups_by_cell_cycle_column" ]],
      mode = "long",
      percent = input[["groups_by_cell_cycle_show_as_percent"]]
    )
    ## generate plot
    plotlyBarChart(
      table = composition_df,
      first_grouping_variable = input[[ "groups_selected_group" ]],
      second_grouping_variable = input[[ "groups_by_cell_cycle_column" ]],
      colors = reactive_colors()[[ input[[ "groups_by_cell_cycle_column" ]] ]],
      percent = input[["groups_by_cell_cycle_show_as_percent"]]
    )
  ##
  } else if ( input[["groups_by_cell_cycle_plot_type"]] == "Sankey plot" ) {
    ## calculate table
    composition_df <- calculateTableAB(
      getMetaData(),
      input[[ "groups_selected_group" ]],
      input[[ "groups_by_cell_cycle_column" ]],
      mode = "long",
      percent = FALSE
    )
    ## get color code for all group levels (from both groups)
    colors_for_groups <- c(
      assignColorsToGroups(composition_df, input[[ "groups_selected_group" ]]),
      assignColorsToGroups(composition_df, input[[ "groups_by_cell_cycle_column" ]])
    )
    ## generate plot
    plotlySankeyPlot(
      table = composition_df,
      first_grouping_variable = input[[ "groups_selected_group" ]],
      second_grouping_variable = input[[ "groups_by_cell_cycle_column" ]],
      colors_for_groups = colors_for_groups
    )
  }
})

##----------------------------------------------------------------------------##
## Table of numbers shown in plot.
##----------------------------------------------------------------------------##

output[["groups_by_cell_cycle_table"]] <- DT::renderDataTable({
  ##
  req(
    input[["groups_selected_group"]] %in% getGroups(),
    input[["groups_by_cell_cycle_column"]] %in% getCellCycle(),
  )
  ##
  composition_df <- calculateTableAB(
    getMetaData(),
    input[["groups_selected_group"]],
    input[["groups_by_cell_cycle_column"]],
    mode = "wide",
    percent = input[["groups_by_cell_cycle_show_as_percent"]]
  )
  ## get indices of columns that should be formatted as percent
  if ( input[["groups_by_cell_cycle_show_as_percent"]] == TRUE ) {
    columns_percentage <- c(3:ncol(composition_df))
  } else {
    columns_percentage <- NULL
  }
  ##
  composition_df %>%
  dplyr::rename("# of cells" = total_cell_count) %>%
  prettifyTable(
    filter = "none",
    dom = "Brtlip",
    show_buttons = FALSE,
    number_formatting = TRUE,
    color_highlighting = FALSE,
    hide_long_columns = TRUE,
    columns_percentage = columns_percentage
  )
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["groups_by_cell_cycle_text"]] <- renderText({
  "No cell cycle assignments available."
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["groups_by_cell_cycle_info"]], {
  showModal(
    modalDialog(
      groups_by_cell_cycle_info[["text"]],
      title = groups_by_cell_cycle_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
groups_by_cell_cycle_info <- list(
  title = "Cell cycle analysis",
  text = p("Shown here is the relationship between the subpopulations of the selected grouping variable and the selected cell cycle assignments. If these assignments were generated with the method embedded in the Seurat framework, for each cell, a score is calculated for both G2M and S phase based on lists of genes (see 'Analysis info' tab on the left). The cell cycle phase is then assigned on the basis of these scores.")
)
