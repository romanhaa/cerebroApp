##----------------------------------------------------------------------------##
## Composition of selected group by other group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["groups_composition_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Composition by other group"),
        cerebroInfoButton("groups_by_other_group_info")
      ),
      tagList(
        uiOutput("groups_by_other_group_other_group_buttons_UI"),
        uiOutput("groups_by_other_group_output_UI")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI elements to select second grouping variable and buttons.
##----------------------------------------------------------------------------##
output[["groups_by_other_group_other_group_buttons_UI"]] <- renderUI({
  req(input[[ "groups_selected_group" ]] %in% getGroups())
  tagList(
    selectInput(
      "groups_by_other_group_second_group",
      label = "Group to compare to:",
      choices = getGroups()[ getGroups() %in% input[[ "groups_selected_group" ]] == FALSE]
    ),
    fluidRow(
      column(
        width = 3,
        shinyWidgets::radioGroupButtons(
           inputId = "groups_by_other_group_plot_type",
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
          inputId = "groups_by_other_group_show_as_percent",
          label = "Show composition as percent [%] (not in Sankey plot):",
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "groups_by_other_group_show_table",
          label = "Show table:",
          status = "primary",
          inline = TRUE
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that shows either just the plot or also the table, depending on
## buttons.
##----------------------------------------------------------------------------##
output[["groups_by_other_group_output_UI"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("groups_by_other_group_plot"),
    {
      if (
        !is.null(input[["groups_by_other_group_show_table"]]) &&
        input[["groups_by_other_group_show_table"]] == TRUE
      ) {
        DT::dataTableOutput("groups_by_other_group_table")
      }
    }
  )
})

##----------------------------------------------------------------------------##
## Plot showing composition of groups, either as a bar chart or a Sankey plot.
##----------------------------------------------------------------------------##
output[["groups_by_other_group_plot"]] <- plotly::renderPlotly({
  ## only proceed if the two groups are not the same (otherwise it can give an
  ## error when switching between groups)
  req(
    input[["groups_selected_group"]] %in% getGroups(),
    input[["groups_by_other_group_second_group"]] %in% getGroups(),
    input[["groups_selected_group"]] != input[["groups_by_other_group_second_group"]],
    input[["groups_by_other_group_plot_type"]]
  )
  ##
  if ( input[["groups_by_other_group_plot_type"]] == "Bar chart" ) {
    ## calculate table
    composition_df <- calculateTableAB(
      getMetaData(),
      input[[ "groups_selected_group" ]],
      input[[ "groups_by_other_group_second_group" ]],
      mode = "long",
      percent = input[["groups_by_other_group_show_as_percent"]]
    )
    ## generate plot
    plotlyBarChart(
      table = composition_df,
      first_grouping_variable = input[[ "groups_selected_group" ]],
      second_grouping_variable = input[[ "groups_by_other_group_second_group" ]],
      colors = reactive_colors()[[ input[[ "groups_by_other_group_second_group" ]] ]],
      percent = input[["groups_by_other_group_show_as_percent"]]
    )
  ##
  } else if ( input[["groups_by_other_group_plot_type"]] == "Sankey plot" ) {
    ## calculate table
    composition_df <- calculateTableAB(
      getMetaData(),
      input[[ "groups_selected_group" ]],
      input[[ "groups_by_other_group_second_group" ]],
      mode = "long",
      percent = FALSE
    )
    ## get color code for all group levels (from both groups)
    colors_for_groups <- c(
      assignColorsToGroups(composition_df, input[[ "groups_selected_group" ]]),
      assignColorsToGroups(composition_df, input[[ "groups_by_other_group_second_group" ]])
    )
    ## generate plot
    plotlySankeyPlot(
      table = composition_df,
      first_grouping_variable = input[[ "groups_selected_group" ]],
      second_grouping_variable = input[[ "groups_by_other_group_second_group" ]],
      colors_for_groups = colors_for_groups
    )
  }
})

##----------------------------------------------------------------------------##
## Table showing numbers of plot.
##----------------------------------------------------------------------------##
output[["groups_by_other_group_table"]] <- DT::renderDataTable({
  ## only proceed if the two groups are not the same (otherwise it can give an
  ## error when switching between groups)
  req(
    input[[ "groups_selected_group" ]],
    input[[ "groups_by_other_group_second_group" ]],
    input[[ "groups_selected_group" ]] != input[[ "groups_by_other_group_second_group" ]]
  )
  ## generate table
  composition_df <- calculateTableAB(
    getMetaData(),
    input[[ "groups_selected_group" ]],
    input[[ "groups_by_other_group_second_group" ]],
    mode = "wide",
    percent = input[["groups_by_other_group_show_as_percent"]]
  )
  ## get indices of columns that should be formatted as percent
  if ( input[["groups_by_other_group_show_as_percent"]] == TRUE ) {
    columns_percentage <- c(3:ncol(composition_df))
  } else {
    columns_percentage <- NULL
  }
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
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["groups_by_other_group_info"]], {
  showModal(
    modalDialog(
      groups_by_other_group_info[["text"]],
      title = groups_by_other_group_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
groups_by_other_group_info <- list(
  title = "Composition of group by another group",
  text = HTML("This plot allows to see how cell groups are related to each other. This can be represented as a bar char or a Sankey plot. Optionally, a table can be shown below. To highlight composition in very small cell groups, results can be shown as percentages rather than actual cell counts. Groups can be removed from the plot by clicking on them in the legend.")
)
