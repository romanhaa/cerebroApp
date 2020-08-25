##----------------------------------------------------------------------------##
## Tab: Groups
##
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

  ##
  req(
    input[["groups_selected_group"]],
    input[["groups_by_cell_cycle_column"]],
    input[["groups_by_cell_cycle_plot_type"]]
  )

  ## calculate table (must be merged later if user chooses to display as percent)
  table_wide <- calculateTableAB(
    getMetaData(),
    input[[ "groups_selected_group" ]],
    input[[ "groups_by_cell_cycle_column" ]]
  )

  ## process table
  table_long <- table_wide %>%
    dplyr::select(-total_cell_count) %>%
    tidyr::pivot_longer(
      cols = 2:ncol(.),
      names_to = input[[ "groups_by_cell_cycle_column" ]],
      values_to = "cells"
    )

  ## factorize second group
  table_long[[ input[[ "groups_by_cell_cycle_column" ]] ]] <- factor(
    table_long[[ input[[ "groups_by_cell_cycle_column" ]] ]],
    levels = unique(table_long[[ input[[ "groups_by_cell_cycle_column" ]] ]])
  )

  ##
  if ( input[["groups_by_cell_cycle_plot_type"]] == "Bar chart" ) {

    ##
    if ( input[["groups_by_cell_cycle_show_as_percent"]] != TRUE ) {
      table_long %>%
      plotly::plot_ly(
        x = ~.[[ input[[ "groups_selected_group" ]] ]],
        y = ~cells,
        type = "bar",
        color = ~.[[ input[[ "groups_by_cell_cycle_column" ]] ]],
        colors = reactive_colors()[[ input[[ "groups_by_cell_cycle_column" ]] ]],
        hoverinfo = "text",
        text = ~paste0("<b>", .[[ input[[ "groups_by_cell_cycle_column" ]] ]], ": </b>", formatC(.$cells, big.mark = ','))
      ) %>%
      plotly::layout(
        xaxis = list(
          title ="",
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
    } else {
      table_long %>%
      dplyr::left_join(
        .data,
        table_wide[ , c(input[[ "groups_selected_group" ]], "total_cell_count") ],
        by = input[[ "groups_selected_group" ]]
      ) %>%
      dplyr::mutate(pct = cells / total_cell_count * 100) %>%
      plotly::plot_ly(
        x = ~.[[ input[[ "groups_selected_group" ]] ]],
        y = ~pct,
        type = "bar",
        color = ~.[[ input[[ "groups_by_cell_cycle_column" ]] ]],
        colors = reactive_colors()[[ input[[ "groups_by_cell_cycle_column" ]] ]],
        hoverinfo = "text",
        text = ~paste0("<b>", .[[ input[[ "groups_by_cell_cycle_column" ]] ]], ": </b>", format(round(.$pct, 1), nsmall = 1), "%")
      ) %>%
      plotly::layout(
        xaxis = list(
          title ="",
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
  } else if ( input[["groups_by_cell_cycle_plot_type"]] == "Sankey plot" ) {

    ## transform factor levels to integers (necessary for plotly)
    table_long[["source"]] <- as.numeric(table_long[[1]]) - 1
    table_long[["target"]] <- as.numeric(table_long[[2]]) - 1 + length(unique(table_long[[1]]))

    ## combine all factor levels in a single vector
    all_groups <- c(levels(table_long[[1]]), levels(table_long[[2]]))

    ## get color code for all group levels (from both groups)
    colors_for_groups <- c(
        reactive_colors()[[ input[[ "groups_selected_group" ]] ]][ levels(table_long[[1]]) ],
        reactive_colors()[[ input[[ "groups_by_cell_cycle_column" ]] ]][ levels(table_long[[2]]) ]
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
        source = table_long[["source"]],
        target = table_long[["target"]],
        value =  table_long[[3]],
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
})

##----------------------------------------------------------------------------##
## Table of numbers shown in plot.
##----------------------------------------------------------------------------##

output[["groups_by_cell_cycle_table"]] <- DT::renderDataTable({

  ##
  req(
    input[["groups_selected_group"]],
    input[["groups_by_cell_cycle_column"]],
  )

  ##
  composition_df <- calculateTableAB(
    getMetaData(),
    input[["groups_selected_group"]],
    input[["groups_by_cell_cycle_column"]]
  )

  ##
  if ( input[["groups_by_cell_cycle_show_as_percent"]] == TRUE ) {
    for ( i in 3:ncol(composition_df) ) {
      composition_df[,i] <- composition_df[,i] / composition_df$total_cell_count
    }
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
