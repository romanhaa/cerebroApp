##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##

output[["groups_selected_group_UI"]] <- renderUI({
  tagList(
    div(
      HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a grouping variable:</strong></h2>')
    ),
    fluidRow(
      column(2),
      column(8,
        selectInput(
          "groups_selected_group",
          label = NULL,
          choices = sample_data()$getGroups(),
          width = "100%"
        )
      ),
      column(2)
    )
  )
})

##----------------------------------------------------------------------------##
## Relationship tree.
##----------------------------------------------------------------------------##

## UI element
output[["groups_tree_UI"]] <- renderUI({
  req(input[["groups_selected_group"]])
  if ( !is.null(sample_data()$getTree( input[["groups_selected_group"]] ) ) ) {
    plotOutput("groups_tree_plot")
  } else {
    textOutput("groups_tree_text")
  }
})

## plot
output[["groups_tree_plot"]] <- renderPlot({
  ## only proceed if tree is present (this check is necessary because it can
  ## otherwise result in an error when switching between groups)
  if ( !is.null(sample_data()$getTree( input[["groups_selected_group"]] ) ) ) {
    ## retrieve tree from Cerebro object
    tree <- sample_data()$getTree( input[["groups_selected_group"]] )
    message(str(tree))
    colors_tree <- reactive_colors()[[ input[["groups_selected_group"]] ]]
    ggtree::ggtree(tree, aes(x, y)) +
      scale_y_reverse() +
      ggtree::geom_tree() +
      ggtree::theme_tree() +
      ggtree::geom_tiplab(size = 5, offset = 1) +
      ggtree::geom_tippoint(color = colors_tree, shape = 16, size = 6) +
      coord_cartesian(clip = 'off') +
      theme(plot.margin = unit(c(0,2.5,0,0), 'cm'))
    }
})

## alternative text
output[["groups_tree_text"]] <- renderText({ "Data not available." })

## info box
observeEvent(input[["groups_tree_info"]], {
  showModal(
    modalDialog(
      groups_tree_info[["text"]],
      title = groups_tree_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Composition by other group: table + bar plot.
##----------------------------------------------------------------------------##

## UI element: buttons
output[["groups_by_other_group_UI_buttons"]] <- renderUI({
  req(
    input[[ "groups_selected_group" ]]
  )
  tagList(
    selectInput(
      "groups_by_other_group_second_group",
      label = "Group to compare to:",
      choices = sample_data()$getGroups()[ sample_data()$getGroups() %in% input[[ "groups_selected_group" ]] == FALSE]
    ),
    shinyWidgets::materialSwitch(
      inputId = "groups_by_other_group_select_metric_for_bar_plot",
      label = "Show composition in percent [%]:",
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
})

## UI element: rest
output[["groups_by_other_group_UI_rest"]] <- renderUI({
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

## bar plot
output[["groups_by_other_group_plot"]] <- plotly::renderPlotly({
  req(
    input[[ "groups_selected_group" ]],
    input[[ "groups_by_other_group_second_group" ]]
  )

  ## only proceed if the two groups are not the same (otherwise it can give an
  ## error when switching between groups)
  if ( input[[ "groups_selected_group" ]] != input[[ "groups_by_other_group_second_group" ]] )
  {

    ## calculate table (must be merged later if user chooses to display in percent)
    temp_table_original <- calculateTableAB(
      input[[ "groups_selected_group" ]],
      input[[ "groups_by_other_group_second_group" ]]
    )

    ## process table
    temp_table_to_plot <- temp_table_original %>%
      select(-total_cell_count) %>%
      tidyr::pivot_longer(
        cols = 2:ncol(.),
        names_to = input[[ "groups_by_other_group_second_group" ]],
        values_to = "cells"
      )

    temp_table_to_plot[[ input[[ "groups_by_other_group_second_group" ]] ]] <- factor(
      temp_table_to_plot[[ input[[ "groups_by_other_group_second_group" ]] ]],
      levels = sample_data()$getGroupLevels(input[[ "groups_by_other_group_second_group" ]])
    )
    if ( input[["groups_by_other_group_select_metric_for_bar_plot"]] != TRUE ) {
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
  }
})

## table
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

    if ( input[["groups_by_other_group_select_metric_for_bar_plot"]] == TRUE ) {
      ## normalize counts to 100% percent
      for ( i in 3:ncol(temp_table) ) {
        temp_table[,i] <- temp_table[,i] / temp_table$total_cell_count
      }
      columns_percentage <- c(3:ncol(temp_table))
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

## info button
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
## nUMI
##----------------------------------------------------------------------------##

# UI element
output[["groups_nUMI_UI"]] <- renderUI({
  if ( "nUMI" %in% colnames(colData(sample_data()$expression)) ) {
    plotly::plotlyOutput("groups_nUMI_plot")
  } else {
    textOutput("groups_nUMI_text")
  }
})

# box plot
output[["groups_nUMI_plot"]] <- plotly::renderPlotly({
  req(
    input[["groups_selected_group"]]
  )
  colData(sample_data()$expression) %>%
  as.data.frame() %>%
  plotly::plot_ly(
    x = ~.[[ input[["groups_selected_group"]] ]],
    y = ~nUMI,
    type = "violin",
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    ),
    color = ~.[[ input[["groups_selected_group"]] ]],
    colors = reactive_colors()[[ input[["groups_selected_group"]] ]],
    source = "subset",
    showlegend = FALSE,
    hoverinfo = "y",
    marker = list(
      size = 5
    )
  ) %>%
  plotly::layout(
    title = "",
    xaxis = list(
      title = "",
      mirror = TRUE,
      showline = TRUE
    ),
    yaxis = list(
      title = "Number of transcripts",
      hoverformat = ".0f",
      mirror = TRUE,
      showline = TRUE
    ),
    dragmode = "select",
    hovermode = "compare"
  )
})

# alternative text
output[["groups_nUMI_text"]] <- renderText({
  "Column with number of transcript per cell not available."
})

# info button
observeEvent(input[["groups_nUMI_info"]], {
  showModal(
    modalDialog(
      groups_nUMI_info[["text"]],
      title = groups_nUMI_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## nGene
##----------------------------------------------------------------------------##

## UI element
output[["groups_nGene_UI"]] <- renderUI({
  if ( "nGene" %in% colnames(colData(sample_data()$expression)) ) {
    plotly::plotlyOutput("groups_nGene_plot")
  } else {
    textOutput("groups_nGene_text")
  }
})

## box plot
output[["groups_nGene_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]])
  colData(sample_data()$expression) %>%
  as.data.frame() %>%
  plotly::plot_ly(
    x = ~.[[ input[["groups_selected_group"]] ]],
    y = ~nGene,
    type = "violin",
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    ),
    color = ~.[[ input[["groups_selected_group"]] ]],
    colors = reactive_colors()[[ input[["groups_selected_group"]] ]],
    source = "subset",
    showlegend = FALSE,
    hoverinfo = "y",
    marker = list(
      size = 5
    )
  ) %>%
  plotly::layout(
    title = "",
    xaxis = list(
      title = "",
      mirror = TRUE,
      showline = TRUE
    ),
    yaxis = list(
      title = "Number of expressed genes",
      hoverformat = ".0f",
      mirror = TRUE,
      showline = TRUE
    ),
    dragmode = "select",
    hovermode = "compare"
  )
})

## alternative text
output[["groups_nGene_text"]] <- renderText({
  "Column with number of expressed genes per cell not available."
})

## info button
observeEvent(input[["groups_nGene_info"]], {
  showModal(
    modalDialog(
      groups_nGene_info[["text"]],
      title = groups_nGene_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## percent_mt
##----------------------------------------------------------------------------##

## UI element
output[["groups_percent_mt_UI"]] <- renderUI({
  if ( "percent_mt" %in% colnames(colData(sample_data()$expression)) ) {
    plotly::plotlyOutput("groups_percent_mt_plot")
  } else {
    textOutput("groups_percent_mt_text")
  }
})

## box plot
output[["groups_percent_mt_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]])
  colData(sample_data()$expression) %>%
  as.data.frame() %>%
  plotly::plot_ly(
    x = ~.[[ input[["groups_selected_group"]] ]],
    y = ~percent_mt*100,
    type = "violin",
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    ),
    color = ~.[[ input[["groups_selected_group"]] ]],
    colors = reactive_colors()[[ input[["groups_selected_group"]] ]],
    source = "subset",
    showlegend = FALSE,
    hoverinfo = "y",
    marker = list(
      size = 5
    )
  ) %>%
  plotly::layout(
    title = "",
    xaxis = list(
      title = "",
      mirror = TRUE,
      showline = TRUE
    ),
    yaxis = list(
      title = "Percentage of transcripts [%]",
      range = c(0,100),
      hoverformat = ".1f",
      mirror = TRUE,
      showline = TRUE
    ),
    dragmode = "select",
    hovermode = "compare"
  )
})

## alternative text
output[["groups_percent_mt_text"]] <- renderText({
  "Column with percentage of mitochondrial expression not available."
})

# info button
observeEvent(input[["groups_percent_mt_info"]], {
  showModal(
    modalDialog(
      groups_percent_mt_info[["text"]],
      title = groups_percent_mt_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## percent_ribo
##----------------------------------------------------------------------------##

## UI element
output[["groups_percent_ribo_UI"]] <- renderUI({
  if ( "percent_ribo" %in% colnames(colData(sample_data()$expression)) ) {
    plotly::plotlyOutput("groups_percent_ribo_plot")
  } else {
    textOutput("groups_percent_ribo_text")
  }
})

## box plot
output[["groups_percent_ribo_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]])
  colData(sample_data()$expression) %>%
  as.data.frame() %>%
  plotly::plot_ly(
    x = ~.[[ input[["groups_selected_group"]] ]],
    y = ~percent_ribo*100,
    type = "violin",
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    ),
    color = ~.[[ input[["groups_selected_group"]] ]],
    colors = reactive_colors()[[ input[["groups_selected_group"]] ]],
    source = "subset",
    showlegend = FALSE,
    hoverinfo = "y",
    marker = list(size = 5)
  ) %>%
  plotly::layout(
    title = "",
    xaxis = list(
      title = "",
      mirror = TRUE,
      showline = TRUE
    ),
    yaxis = list(
      title = "Percentage of transcripts [%]",
      range = c(0,100),
      hoverformat = ".1f",
      mirror = TRUE,
      showline = TRUE
    ),
    dragmode = "select",
    hovermode = "compare"
  )
})

## alternative text
output[["groups_percent_ribo_text"]] <- renderText({
  "Column with percentage of ribosomal expression not available."
})

## info button
observeEvent(input[["groups_percent_ribo_info"]], {
  showModal(
    modalDialog(
      groups_percent_ribo_info[["text"]],
      title = groups_percent_ribo_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## cell cycle
##----------------------------------------------------------------------------##

## UI element: buttons
output[["groups_by_cell_cycle_UI_buttons"]] <- renderUI({
  if ( length(sample_data()$cell_cycle) > 0 ) {
    tagList(
      selectInput(
        "groups_by_cell_cycle_column",
        label = "Column to take data from:",
        choices = sample_data()$cell_cycle
      ),
      shinyWidgets::materialSwitch(
        inputId = "groups_by_cell_cycle_select_metric_for_bar_plot",
        label = "Show composition in percent [%]:",
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
  } else {
    textOutput("groups_by_cell_cycle_text")
  }
})

## UI element: rest
output[["groups_by_cell_cycle_UI_rest"]] <- renderUI({
  if ( length(sample_data()$cell_cycle) > 0 ) {
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

## bar plot
output[["groups_by_cell_cycle_plot"]] <- plotly::renderPlotly({
  req(
    input[[ "groups_selected_group" ]],
    input[[ "groups_by_cell_cycle_column" ]]
  )

  temp_table_original <- calculateTableAB(
    input[[ "groups_selected_group" ]],
    input[[ "groups_by_cell_cycle_column" ]]
  )

  temp_table_to_plot <- temp_table_original %>%
    select(-total_cell_count) %>%
    tidyr::pivot_longer(
      cols = 2:ncol(.),
      names_to = input[[ "groups_by_cell_cycle_column" ]],
      values_to = "cells"
    )

  if ( input[["groups_by_cell_cycle_select_metric_for_bar_plot"]] != TRUE ) {
    temp_table_to_plot %>%
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
})

## table
output[["groups_by_cell_cycle_table"]] <- DT::renderDataTable({
  req(
    input[[ "groups_selected_group" ]],
    input[[ "groups_by_cell_cycle_column" ]],
  )

  temp_table <- calculateTableAB(
    input[[ "groups_selected_group" ]],
    input[[ "groups_by_cell_cycle_column" ]]
  )

  if ( input[["groups_by_cell_cycle_select_metric_for_bar_plot"]] == TRUE ) {
    for ( i in 3:ncol(temp_table) ) {
      # temp_table[,i] <- round(temp_table[,i] / temp_table$total_cell_count * 100, digits = 1)
      temp_table[,i] <- temp_table[,i] / temp_table$total_cell_count
    }
    columns_percentage <- c(3:ncol(temp_table))
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

})

## alternative text
output[["groups_by_cell_cycle_text"]] <- renderText({
  "Data not available."
})

## info button
observeEvent(input[["groups_by_cell_cycle_info"]], {
  showModal(
    modalDialog(
      groups_by_cell_cycle_info[["text"]],
      title = groups_by_cell_cycle_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

