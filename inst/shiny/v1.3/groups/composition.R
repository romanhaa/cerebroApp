##----------------------------------------------------------------------------##
## Tab: Groups
##
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

  ##
  req(
    input[[ "groups_selected_group" ]]
  )

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

  ##
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

  ##
  req(
    input[["groups_selected_group"]],
    input[["groups_by_other_group_second_group"]],
    input[["groups_by_other_group_plot_type"]]
  )

  ## only proceed if the two groups are not the same (otherwise it can give an
  ## error when switching between groups)
  if ( input[[ "groups_selected_group" ]] != input[[ "groups_by_other_group_second_group" ]] ) {

    ## calculate table (must be merged later if user chooses to display as percent)
    temp_table_original <- calculateTableAB(
      input[[ "groups_selected_group" ]],
      input[[ "groups_by_other_group_second_group" ]]
    )

    ## process table
    temp_table_to_plot <- temp_table_original %>%
      dplyr::select(-total_cell_count) %>%
      tidyr::pivot_longer(
        cols = 2:ncol(.),
        names_to = input[[ "groups_by_other_group_second_group" ]],
        values_to = "cells"
      )

    ## factorize second group
    temp_table_to_plot[[ input[[ "groups_by_other_group_second_group" ]] ]] <- factor(
      temp_table_to_plot[[ input[[ "groups_by_other_group_second_group" ]] ]],
      levels = getGroupLevels(input[[ "groups_by_other_group_second_group" ]])
    )

    ##
    if ( input[["groups_by_other_group_plot_type"]] == "Bar chart" ) {

      ##
      if ( input[["groups_by_other_group_show_as_percent"]] != TRUE ) {

        ## generate bar plot with actual cell counts
        temp_table_to_plot %>%
        plotly::plot_ly(
          x = ~.[[ input[[ "groups_selected_group" ]] ]],
          y = ~cells,
          type = "bar",
          color = ~.[[ input[[ "groups_by_other_group_second_group" ]] ]],
          colors = reactive_colors()[[ input[[ "groups_by_other_group_second_group" ]] ]],
          hoverinfo = "text",
          text = ~paste0(
            "<b>", .[[ input[[ "groups_by_other_group_second_group" ]] ]],
            ": </b>", formatC(.$cells, big.mark = ',')
          )
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

      ##
      } else {

        ## normalize counts to 100% and generate bar plot in percent
        temp_table_to_plot %>%
        left_join(
          .,
          temp_table_original[ , c(input[[ "groups_selected_group" ]], "total_cell_count") ],
          by = input[[ "groups_selected_group" ]]
        ) %>%
        mutate(pct = cells / total_cell_count * 100) %>%
        plotly::plot_ly(
          x = ~.[[ input[[ "groups_selected_group" ]] ]],
          y = ~pct,
          type = "bar",
          color = ~.[[ input[[ "groups_by_other_group_second_group" ]] ]],
          colors = reactive_colors()[[ input[[ "groups_by_other_group_second_group" ]] ]],
          hoverinfo = "text",
          text = ~paste0(
            "<b>", .[[ input[[ "groups_by_other_group_second_group" ]] ]],
            ": </b>", format(round(.$pct, 1), nsmall = 1), "%"
          )
        ) %>%
        plotly::layout(
          xaxis = list(
            title = "",
            mirror = TRUE,
            showline = TRUE
          ),
          yaxis = list(
            title = "Percent of cells [%]",
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

    ##
    } else if ( input[["groups_by_other_group_plot_type"]] == "Sankey plot" ) {

      ## transform factor levels to integers (necessary for plotly)
      temp_table_to_plot[["source"]] <- as.numeric(temp_table_to_plot[[1]]) - 1
      temp_table_to_plot[["target"]] <- as.numeric(temp_table_to_plot[[2]]) - 1 + length(unique(temp_table_to_plot[[1]]))

      ## combine all factor levels in a single vector
      all_groups <- c(levels(temp_table_to_plot[[1]]), levels(temp_table_to_plot[[2]]))

      ## get color code for all group levels (from both groups)
      colors_for_groups <- c(
          reactive_colors()[[ input[[ "groups_selected_group" ]] ]][ levels(temp_table_to_plot[[1]]) ],
          reactive_colors()[[ input[[ "groups_by_other_group_second_group" ]] ]][ levels(temp_table_to_plot[[2]]) ]
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
          source = temp_table_to_plot[["source"]],
          target = temp_table_to_plot[["target"]],
          value =  temp_table_to_plot[[3]],
          hoverinfo = "all",
          hovertemplate = paste0(
            "<b>", input[["groups_selected_group"]], ":</b> %{source.label}<br>",
            "<b>", input[["groups_by_other_group_second_group"]], ":</b> %{target.label}<br>",
            "<b>Number of cells:</b> %{value:,.0f}",
            "<extra></extra>",
            collapse = ""
          )
        )
      )
    }
  }
})

##----------------------------------------------------------------------------##
## Table showing numbers of plot.
##----------------------------------------------------------------------------##

output[["groups_by_other_group_table"]] <- DT::renderDataTable({
  req(
    input[[ "groups_selected_group" ]],
    input[[ "groups_by_other_group_second_group" ]]
  )

  ## only proceed if the two groups are not the same (otherwise it can give an
  ## error when switching between groups)
  if ( input[[ "groups_selected_group" ]] != input[[ "groups_by_other_group_second_group" ]] )
  {

    ## generate table
    temp_table <- calculateTableAB(
      input[[ "groups_selected_group" ]],
      input[[ "groups_by_other_group_second_group" ]]
    )

    ##
    if ( input[["groups_by_other_group_show_as_percent"]] == TRUE ) {

      ## normalize counts to 100% percent
      for ( i in 3:ncol(temp_table) ) {
        temp_table[,i] <- temp_table[,i] / temp_table$total_cell_count
      }

      ##
      columns_percentage <- c(3:ncol(temp_table))

    ##
    } else {
      columns_percentage <- NULL
    }

    temp_table %>%
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

  }
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
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

## TODO: update description
groups_by_other_group_info <- list(
  title = "Samples by cluster",
  text = p("Percentage bar plot representation of the table shown above. Allows to see which groups contribute most strongly to each sample. Groups can be removed from the plot by clicking on them in the legend.")
)
