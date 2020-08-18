##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## States by group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_states_by_group_UI"]] <- renderUI({
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
        uiOutput("states_by_group_table_UI")
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
    input[["trajectory_point_color"]],
    input[["states_by_group_plot_type"]]
  )

  ## collect trajectory data
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
  
  ## merge trajectory data with meta data
  temp_data <- cbind(
    trajectory_data[["meta"]],
    getMetaData()
  )

  ## if the selected variable to color cells by contains numeric values, show
  ## cell counts by state, otherwise split the cell counts by the selected
  ## (categorical) variable
  if ( is.numeric(temp_data[[ input[["trajectory_point_color"]] ]]) ) {
    grouping_variable <- "state"
  } else {
    grouping_variable <- input[["trajectory_point_color"]]
  }

  ## - remove cells without pseudotime (NA)
  ## - group table by state and selected variable
  ## - count number of cells for each combination of groups
  ## - add total cell count per state to each combination of groups
  temp_data <- temp_data %>%
    dplyr::filter(!is.na(pseudotime)) %>%
    dplyr::group_by_at(c("state", grouping_variable)) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::ungroup()

  ## if the selected grouping variable is present in reactive_colors(), take
  ## color assignments from there, otherwise use default_colorset
  if ( grouping_variable %in% names(reactive_colors()) ) {
    colors_this_plot <- reactive_colors()[[ grouping_variable ]]
  } else {
    if ( is.factor(temp_data[[ grouping_variable ]]) ) {
      colors_this_plot <- setNames(
        default_colorset[1:length(levels(temp_data[[ grouping_variable ]]))],
        levels(temp_data[[ grouping_variable ]])
      )
    } else if ( is.character(temp_data[[ grouping_variable ]]) ) {
      colors_this_plot <- setNames(
        default_colorset[1:length(unique(temp_data[[ grouping_variable ]]))],
        unique(temp_data[[ grouping_variable ]])
      )
    }
  }

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
      temp_data %>%
      plotly::plot_ly(
        x = ~state,
        y = ~count,
        type = "bar",
        color = temp_data[[ grouping_variable ]],
        colors = colors_this_plot,
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
      temp_data %>%
      dplyr::mutate(pct = count / total * 100) %>%
      plotly::plot_ly(
        x = ~state,
        y = ~pct,
        type = "bar",
        color = temp_data[[ grouping_variable ]],
        colors = colors_this_plot,
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
    temp_data[["source"]] <- as.numeric(temp_data[[1]]) - 1
    temp_data[["target"]] <- as.numeric(temp_data[[2]]) - 1 + length(unique(temp_data[[1]]))

    ## combine all factor levels in a single vector
    all_groups <- c(levels(temp_data[[1]]), levels(temp_data[[2]]))

    ##
    colors_states <- setNames(
      default_colorset[1:length(levels(trajectory_data[["meta"]]$state))],
      levels(trajectory_data[["meta"]]$state)
    )

    ## get color code for all group levels (from both groups)
    colors_for_groups <- c(
        colors_states,
        colors_this_plot
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
        source = temp_data[["source"]],
        target = temp_data[["target"]],
        value =  temp_data[[3]],
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
  temp_data <- cbind(
    trajectory_data[["meta"]],
    getMetaData()
  )

  ## if the selected variable to color cells by contains numeric values, show
  ## cell counts by state, otherwise split the cell counts by the selected
  ## (categorical) variable
  if ( is.numeric(temp_data[[ input[["trajectory_point_color"]] ]]) ) {
    grouping_variable <- "state"
  } else {
    grouping_variable <- input[["trajectory_point_color"]]
  }

  ## - remove cells without pseudotime (NA)
  ## - group table by state and selected variable
  ## - count number of cells for each combination of groups
  ## - add total cell count per state to each combination of groups
  temp_data <- temp_data %>%
    dplyr::filter(!is.na(pseudotime)) %>%
    dplyr::group_by_at(c("state", grouping_variable)) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::ungroup()

  ## if percentages should be shown, normalize counts (to 100%)
  if ( input[["states_by_group_show_as_percent"]] == TRUE ) {
    temp_data <- dplyr::mutate(temp_data, count = count / total)
  }

  ## if grouping variable is not "state", spread the table by grouping variable
  if ( grouping_variable != "state" ) {
    temp_data <- temp_data %>%
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
      temp_data <- temp_data %>%
        dplyr::select(state, total, dplyr::everything()) %>%
        dplyr::rename("Percent" = count)

    ## if cell counts should be shown, remove "count" column
    } else {
      temp_data <- temp_data %>%
        dplyr::select(-count)
    }
  }

  ## if percentages should be shown, set which columns contain percentage values,
  ## otherwise create empty vector
  if ( input[["states_by_group_show_as_percent"]] == TRUE ) {
    columns_percentage <- c(3:ncol(temp_data))
  } else {
    columns_percentage <- NULL
  }

  ## rename columns and create table
  temp_data %>%
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
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

## TODO: update
states_by_group_info <- list(
  title = "Composition of states by sample",
  text = p("Percentage bar plot representation of the composition of states by sample. Allows to see which samples contribute most strongly to each state. Samples can be removed from the plot by clicking on them in the legend.")
)

trajectory_number_of_cells_by_state_info <- list(
  title = "Number of cells by state",
  text = p("This table shows how many cells are assigned to each state. If the variable selected to color the cells in the projection is categorical, e.g. 'sample' or 'cluster', the number of cells in each state is also split into the subgroups.")
)

states_by_cell_cycle_seurat_info <- list(
  title = "Composition of states by cell cycle (Seurat)",
  text = p("Cell cycle distribution by sample using the method embedded in the Seurat framework. For each cell, it calculates scores for both G2M and S phase based on lists of genes (see 'Analysis info' tab on the left) and assigns the cell cycle phase on the basis of these scores.")
)
