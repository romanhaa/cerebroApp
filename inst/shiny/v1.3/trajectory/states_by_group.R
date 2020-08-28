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

  ## - remove cells without pseudotime (NA)
  ## - group table by state and selected variable
  ## - count number of cells for each combination of groups
  ## - add total cell count per state to each combination of groups
  cells_df <- cells_df %>%
    dplyr::filter(!is.na(pseudotime)) %>%
    dplyr::group_by_at(c("state", grouping_variable)) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::ungroup()

  ## get colors for groups
  colors_for_groups <- assignColorsToGroups(cells_df, grouping_variable)

  ## check which plot type is chosen
  ## ... bar chart
  if (
    input[["states_by_group_plot_type"]] == "Bar chart" ||
    grouping_variable == "state"
  ) {

    ## check whether cell counts or percentages should be shown
    ## ... cell counts
    if ( input[["states_by_group_show_as_percent"]] != TRUE ) {

      ## prepare plot
      cells_df %>%
      plotly::plot_ly(
        x = ~state,
        y = ~count,
        type = "bar",
        color = cells_df[[ grouping_variable ]],
        colors = colors_for_groups,
        hoverinfo = "text",
        text = ~paste0("<b>", .[[2]], ":</b> ", formatC(.$count, big.mark = ','))
      ) %>%
      plotly::layout(
        xaxis = list(
          title = "",
          mirror = TRUE,
          showline = TRUE
        ),
        yaxis = list(
          title = "Number of cells",
          hoverformat = ".2f",
          mirror = TRUE,
          zeroline = FALSE,
          showline = TRUE
        ),
        barmode = "stack",
        hovermode = "compare"
      )

    ## ... percentages
    } else {

      ## - convert count to percentages
      ## - prepare plot
      cells_df %>%
      dplyr::mutate(pct = count / total * 100) %>%
      plotly::plot_ly(
        x = ~state,
        y = ~pct,
        type = "bar",
        color = cells_df[[ grouping_variable ]],
        colors = colors_for_groups,
        hoverinfo = "text",
        text = ~paste0("<b>", .[[2]], ":</b> ", format(round(.$pct, 1), nsmall = 1), "%")
      ) %>%
      plotly::layout(
        xaxis = list(
          title = "",
          mirror = TRUE,
          showline = TRUE
        ),
        yaxis = list(
          title = "Percentage (%)",
          range = c(0,100),
          hoverformat = ".2f",
          mirror = TRUE,
          zeroline = FALSE,
          showline = TRUE
        ),
        barmode = "stack",
        hovermode = "compare"
      )
    }

  ## ... Sankey plot
  } else if ( input[["states_by_group_plot_type"]] == "Sankey plot" ) {

    ## transform factor levels to integers (necessary for plotly)
    cells_df[["source"]] <- as.numeric(cells_df[[1]]) - 1
    cells_df[["target"]] <- as.numeric(cells_df[[2]]) - 1 + length(unique(cells_df[[1]]))

    ## combine all factor levels in a single vector
    all_groups <- c(levels(cells_df[[1]]), levels(cells_df[[2]]))

    ##
    colors_states <- setNames(
      default_colorset[seq_along(levels(trajectory_data[["meta"]]$state))],
      levels(trajectory_data[["meta"]]$state)
    )

    ## get color code for all group levels (from both groups)
    colors_for_groups <- c(
        colors_states,
        colors_for_groups
      )

    ## match color codes to group levels (from both groups)
    colors_for_groups_all <- colors_for_groups[names(colors_for_groups) %in% all_groups]

    ## prepare plot
    plotly::plot_ly(
      type = "sankey",
      orientation = "v",
      valueformat = ".0f",
      node = list(
        label = all_groups,
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          "%{value:,.0f} cells",
          "<extra></extra>",
          collapse = ""
        ),
        color = colors_for_groups_all,
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      link = list(
        source = cells_df[["source"]],
        target = cells_df[["target"]],
        value =  cells_df[[3]],
        hoverinfo = "all",
        hovertemplate = paste0(
          "<b>State:</b> %{source.label}<br>",
          "<b>", grouping_variable, ":</b> %{target.label}<br>",
          "<b>Number of cells:</b> %{value:,.0f}",
          "<extra></extra>",
          collapse = ""
        )
      )
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
    input[["trajectory_point_color"]]
  )

  ## collect trajectory data
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
  
  ## merge trajectory data with meta data
  cells_df <- cbind(trajectory_data[["meta"]], getMetaData())

  ## if the selected variable to color cells by contains numeric values, show
  ## cell counts by state, otherwise split the cell counts by the selected
  ## (categorical) variable
  if ( is.numeric(cells_df[[ input[["trajectory_point_color"]] ]]) ) {
    grouping_variable <- "state"
  } else {
    grouping_variable <- input[["trajectory_point_color"]]
  }

  ## - remove cells without pseudotime (NA)
  ## - group table by state and selected variable
  ## - count number of cells for each combination of groups
  ## - add total cell count per state to each combination of groups
  cells_df <- cells_df %>%
    dplyr::filter(!is.na(pseudotime)) %>%
    dplyr::group_by_at(c("state", grouping_variable)) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::ungroup()

  ## if percentages should be shown, normalize counts (to 100%)
  if ( input[["states_by_group_show_as_percent"]] == TRUE ) {
    cells_df <- dplyr::mutate(cells_df, count = count / total)
  }

  ## if grouping variable is not "state", spread the table by grouping variable
  if ( grouping_variable != "state" ) {
    cells_df <- cells_df %>%
      tidyr::pivot_wider(
        id_cols = c("state", "total"),
        names_from = 2,
        values_from = count,
        values_fill = 0
      )

  ## if grouping variable is "state"
  } else {

    ## if percentages should be shown, reorder and rename columns
    if ( input[["states_by_group_show_as_percent"]] == TRUE ) {
      cells_df <- cells_df %>%
        dplyr::select(state, total, dplyr::everything()) %>%
        dplyr::rename("Percent" = count)

    ## if cell counts should be shown, remove "count" column
    } else {
      cells_df <- cells_df %>%
        dplyr::select(-count)
    }
  }

  ## if percentages should be shown, set which columns contain percentage values,
  ## otherwise create empty vector
  if ( input[["states_by_group_show_as_percent"]] == TRUE ) {
    columns_percentage <- c(3:ncol(cells_df))
  } else {
    columns_percentage <- NULL
  }

  ## rename columns and create table
  cells_df %>%
  dplyr::rename(
    State = state,
    "# of cells" = total
  ) %>%
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
