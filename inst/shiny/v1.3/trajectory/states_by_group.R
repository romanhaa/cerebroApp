##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## States by group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_states_by_group_UI"]] <- renderUI({

  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("States by group"),
        cerebroInfoButton("states_by_group_info")
      ),
      tagList(
        fluidRow(
          column(
            width = 3,
            shinyWidgets::radioGroupButtons(
               inputId = "states_by_group_plot_type",
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
              inputId = "states_by_group_show_as_percent",
              label = "Show composition as percent [%]:",
              status = "primary",
              inline = TRUE
            ),
            shinyWidgets::materialSwitch(
              inputId = "states_by_group_show_table",
              label = "Show table:",
              status = "primary",
              inline = TRUE
            )
          )
        ),
        tagList(
          selectInput(
            "states_by_group_select_other_group",
            label = "Group to compare to:",
            choices = c(getGroups(), getCellCycle())
          ),
          uiOutput("states_by_group_table_UI")
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows just the plot or also a table.
##----------------------------------------------------------------------------##

output[["states_by_group_table_UI"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("states_by_group_plot"),
    {
      if (
        !is.null(input[["states_by_group_show_table"]]) &&
        input[["states_by_group_show_table"]] == TRUE
      ) {
        DT::dataTableOutput("states_by_group_table")
      }
    }
  )
})

##----------------------------------------------------------------------------##
## Plot.
##----------------------------------------------------------------------------##

output[["states_by_group_plot"]] <- plotly::renderPlotly({

  ## wait for this input
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]],
    input[["states_by_group_select_other_group"]],
    input[["states_by_group_plot_type"]]
  )

  ## collect trajectory data
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
  
  ## merge trajectory data with meta data
  cells_df <- cbind(trajectory_data[["meta"]], getMetaData())

  ##
  grouping_variable <- input[["states_by_group_select_other_group"]]

  ## check which plot type is chosen
  ## ... bar chart
  if (
    input[["states_by_group_plot_type"]] == "Bar chart"
  ) {

    ##
    composition_df <- cells_df %>%
      dplyr::filter(!is.na(pseudotime)) %>%
      .calculateTableAB(
        "state",
        grouping_variable,
        mode = "long",
        percent = input[["states_by_group_show_as_percent"]]
      )

    ## get colors for groups
    colors_for_groups <- .assignColorsToGroups(composition_df, grouping_variable, reactive_colors())

    ## generate plot
    plotlyBarChart(
      table = composition_df,
      first_grouping_variable = "state",
      second_grouping_variable = grouping_variable,
      colors = colors_for_groups,
      percent = input[["states_by_group_show_as_percent"]]
    )

  ## ... Sankey plot
  } else if ( input[["states_by_group_plot_type"]] == "Sankey plot" ) {

    ##
    composition_df <- cells_df %>%
      dplyr::filter(!is.na(pseudotime)) %>%
      .calculateTableAB(
        "state",
        grouping_variable,
        mode = "long",
        percent = FALSE
      )

    ## get color code for all group levels (from both groups)
    colors_for_groups <- c(
      .assignColorsToGroups(composition_df, "state", reactive_colors()),
      .assignColorsToGroups(composition_df, grouping_variable, reactive_colors())
    )

    ## generate plot
    plotlySankeyPlot(
      table = composition_df,
      first_grouping_variable = "state",
      second_grouping_variable = grouping_variable,
      colors_for_groups = colors_for_groups
    )
  }
})

##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##

output[["states_by_group_table"]] <- DT::renderDataTable({

  ## wait for these inputs
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]],
    input[["states_by_group_select_other_group"]]
  )

  ## collect trajectory data
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
  
  ## merge trajectory data with meta data
  cells_df <- cbind(trajectory_data[["meta"]], getMetaData())

  ##
  grouping_variable <- input[["states_by_group_select_other_group"]]

  ## generate table
  composition_df <- cells_df %>%
    dplyr::filter(!is.na(pseudotime)) %>%
    .calculateTableAB(
      "state",
      grouping_variable,
      mode = "wide",
      percent = input[["states_by_group_show_as_percent"]]
    )

  ## get indices of columns that should be formatted as percent
  if ( input[["states_by_group_show_as_percent"]] == TRUE ) {
    columns_percentage <- c(3:ncol(composition_df))
  } else {
    columns_percentage <- NULL
  }

  composition_df %>%
  dplyr::rename("# of cells" = total_cell_count) %>%
  .prettifyTable(
    getGroups(),
    getCellCycle(),
    reactive_colors(),
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

observeEvent(input[["states_by_group_info"]], {
  showModal(
    modalDialog(
      states_by_group_info[["text"]],
      title = states_by_group_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

states_by_group_info <- list(
  title = "Composition of states by another group",
  text = p("This plot allows to see how state assignments relate to other cells groupings, e.g. clusters or cell cycle assignments. The grouping that the states are compared to depends on the variable that was selected to color cells in the projection plot. If you selected a continuous variable, e.g. the number of transcripts per cell (nUMI), then only the states will be shown. The composition of states by another group can be represented as a bar char or a Sankey plot. Optionally, a table can be shown below. To highlight composition in very small cell groups, results can be shown as percentages rather than actual cell counts. Groups can be removed from the plot by clicking on them in the legend.")
)
