##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##
## Expression in projection.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Function to plot expression of multiple genes in separate facets.
##----------------------------------------------------------------------------##

plotExpressionMultiplePanels <- function(table) {

  ## make ggplot2 functions available
  require("ggplot2")

  ## decide how many panel columns should be used
  ## below 6 panels, use 2 columns, from 6-8 panels use 3 columns
  number_of_genes <- length(unique(gene_expression_plot_data()$gene))
  number_of_panel_columns <- ifelse(number_of_genes < 6, 2, 3)

  ## get X and Y scale limits
  xlim <- c(
    input[["expression_projection_scale_x_manual_range"]][1],
    input[["expression_projection_scale_x_manual_range"]][2]
  )
  ylim <- c(
    input[["expression_projection_scale_y_manual_range"]][1],
    input[["expression_projection_scale_y_manual_range"]][2]
  )

  ## prepare plot
  plot <- ggplot(
      table,
      aes_q(
        x = as.name(colnames(table)[1]),
        y = as.name(colnames(table)[2]),
        color = as.name("level")
      )
    ) +
    geom_point(
      size = input[["expression_projection_point_size"]]/10,
      alpha = input[["expression_projection_point_opacity"]]
    ) +
    lims(x = xlim, y = ylim) +
    theme_bw() +
    facet_wrap(~gene, ncol = number_of_panel_columns)

  ## check if selected color scale
  ## ... selected color scale is "viridis"
  if ( input[["expression_projection_color_scale"]] == 'viridis' ) {

    ## add color scale to plot
    plot <- plot +
      viridis::scale_color_viridis(
        option = "viridis",
        limits = input[["expression_projection_color_scale_range"]],
        oob = scales::squish,
        direction = -1,
        name = "Log-normalised\nexpression",
        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
      )

  ## ... selected color scale is anything else than "viridis"
  } else {

    ## add color scale to plot
    plot <- plot +
      scale_color_distiller(
        palette = input[["expression_projection_color_scale"]],
        limits = input[["expression_projection_color_scale_range"]],
        oob = scales::squish,
        direction = 1,
        name = "Log-normalised\nexpression",
        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
      )
  }

  ##
  return(plot)
}

##----------------------------------------------------------------------------##
## Function to plot expression in single panel in 3D.
##----------------------------------------------------------------------------##

plotExpressionSinglePanel3D <- function(table, color_scale, hover_info) {

  ## prepare plot
  plot <- plotly::plot_ly(
      table,
      x = table[,1],
      y = table[,2],
      z = table[,3],
      type = "scatter3d",
      mode = "markers",
      marker = list(
        colorbar = list(
          title = "Expression"
        ),
        color = ~level,
        opacity = input[["expression_projection_point_opacity"]],
        colorscale = color_scale,
        cauto = FALSE,
        cmin = input[["expression_projection_color_scale_range"]][1],
        cmax = input[["expression_projection_color_scale_range"]][2],
        reversescale = TRUE,
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["expression_projection_point_size"]]
      ),
      hoverinfo = "text",
      text = ~hover_info,
      source = "expression_projection"
    ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(
          title = colnames(table)[1],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = colnames(table)[2],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        zaxis = list(
          title = colnames(table)[3],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        )
      ),
      hoverlabel = list(
        font = list(
          size = 11,
          color = "black"
        ),
        bgcolor = "lightgrey"
      )
    )

  ##
  return(plot)
}

##----------------------------------------------------------------------------##
## Function to plot expression in single panel in 2D.
##----------------------------------------------------------------------------##

plotExpressionSinglePanel2D <- function(table, color_scale, hover_info) {

  ## prepare plot
  plot <- plotly::plot_ly(
      table,
      x = table[,1],
      y = table[,2],
      type = "scatter",
      mode = "markers",
      marker = list(
        colorbar = list(
          title = "Expression"
        ),
        color = ~level,
        opacity = input[["expression_projection_point_opacity"]],
        colorscale = color_scale,
        cauto = FALSE,
        cmin = input[["expression_projection_color_scale_range"]][1],
        cmax = input[["expression_projection_color_scale_range"]][2],
        reversescale = TRUE,
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["expression_projection_point_size"]]
      ),
      hoverinfo = "text",
      text = ~hover_info,
      source = "expression_projection"
    ) %>%
    plotly::layout(
      xaxis = list(
        title = colnames(table)[1],
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = c(
          input[["expression_projection_scale_x_manual_range"]][1],
          input[["expression_projection_scale_x_manual_range"]][2]
        )
      ),
      yaxis = list(
        title = colnames(table)[2],
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = c(
          input[["expression_projection_scale_y_manual_range"]][1],
          input[["expression_projection_scale_y_manual_range"]][2]
        )
      ),
      dragmode = "pan",
      hoverlabel = list(
        font = list(
          size = 11,
          color = "black"
        ),
        bgcolor = "lightgrey"
      )
    )

  ##
  return(plot)
}

##----------------------------------------------------------------------------##
## Function to plot expression in trajectory.
##----------------------------------------------------------------------------##

plotExpressionSinglePanel2DTrajectory <- function(
  table,
  trajectory_edges,
  color_scale,
  hover_info
) {

  ## convert edges of trajectory into list format to plot with plotly
  trajectory_lines <- list()
  for (i in 1:nrow(trajectory_edges) ) {
    line = list(
      type = "line",
      line = list(color = "black"),
      xref = "x",
      yref = "y",
      x0 = trajectory_edges$source_dim_1[i],
      y0 = trajectory_edges$source_dim_2[i],
      x1 = trajectory_edges$target_dim_1[i],
      y1 = trajectory_edges$target_dim_2[i]
    )
    trajectory_lines <- c(trajectory_lines, list(line))
  }

  ##
  plot <- plotly::plot_ly(
      data = table,
      x = ~DR_1,
      y = ~DR_2,
      type = "scatter",
      mode = "markers",
      marker = list(
        colorbar = list(
          title = "Expression"
        ),
        color = ~level,
        opacity = input[["expression_projection_point_opacity"]],
        colorscale = color_scale,
        cauto = FALSE,
        cmin = input[["expression_projection_color_scale_range"]][1],
        cmax = input[["expression_projection_color_scale_range"]][2],
        reversescale = TRUE,
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["expression_projection_point_size"]]
      ),
      hoverinfo = "text",
      text = ~hover_info,
      source = "expression_projection"
    ) %>%
    plotly::layout(
      shapes = trajectory_lines,
      xaxis = list(
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = c(
          input[["expression_projection_scale_x_manual_range"]][1],
          input[["expression_projection_scale_x_manual_range"]][2]
        )
      ),
      yaxis = list(
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE,
        range = c(
          input[["expression_projection_scale_y_manual_range"]][1],
          input[["expression_projection_scale_y_manual_range"]][2]
        )
      ),
      hoverlabel = list(font = list(size = 11))
    )

  ##
  return(plot)
}

##----------------------------------------------------------------------------##
## Function to plot expression in single panel in 2D (for export).
##----------------------------------------------------------------------------##

plotExpressionSinglePanel2DExport <- function(table) {

  ## make ggplot2 functions available
  require("ggplot2")

  ## get X and Y scale limits
  xlim <- c(
    input[["expression_projection_scale_x_manual_range"]][1],
    input[["expression_projection_scale_x_manual_range"]][2]
  )
  ylim <- c(
    input[["expression_projection_scale_y_manual_range"]][1],
    input[["expression_projection_scale_y_manual_range"]][2]
  )

  ## prepare plot
  plot <- ggplot(
      table,
      aes_q(
        x = as.name(colnames(table)[1]),
        y = as.name(colnames(table)[2]),
        fill = as.name("level")
      )
    ) +
    geom_point(
      shape = 21,
      size = input[["expression_projection_point_size"]]/3,
      stroke = 0.2,
      color = "#c4c4c4",
      alpha = input[["expression_projection_point_opacity"]]
    ) +
    lims(x = xlim, y = ylim) +
    theme_bw()

  ## check if selected color scale
  ## ... selected color scale is "viridis"
  if ( input[["expression_projection_color_scale"]] == 'viridis' ) {

    ## add color scale to plot
    plot <- plot +
      viridis::scale_fill_viridis(
        option = "viridis",
        limits = input[["expression_projection_color_scale_range"]],
        oob = scales::squish,
        direction = -1,
        name = "Log-normalised\nexpression",
        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
      )

  ## ... selected color scale is anything else than "viridis"
  } else {

    ## add color scale to plot
    plot <- plot +
      scale_fill_distiller(
        palette = input[["expression_projection_color_scale"]],
        limits = input[["expression_projection_color_scale_range"]],
        oob = scales::squish,
        direction = 1,
        name = "Log-normalised\nexpression",
        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
      )
  }

  ##
  return(plot)
}

##----------------------------------------------------------------------------##
## Function to plot expression in trajectory (for export).
##----------------------------------------------------------------------------##

plotExpressionSinglePanel2DTrajectoryExport <- function(
  table,
  trajectory_edges
) {

  ## start building the plot
  plot <- ggplot() +
    geom_point(
      data = table,
      aes_string(
        x = colnames(table)[1],
        y = colnames(table)[2],
        fill = as.name("level")
      ),
      shape = 21,
      size = input[["expression_projection_point_size"]]/3,
      stroke = 0.2,
      color = "#c4c4c4",
      alpha = input[["expression_projection_point_opacity"]]
    ) +
    geom_segment(
      data = trajectory_edges,
      aes(
        source_dim_1,
        source_dim_2,
        xend = target_dim_1,
        yend = target_dim_2
      ),
      size = 0.75, linetype = "solid", na.rm = TRUE
    ) +
    theme_bw()

  ## check if selected color scale
  ## ... selected color scale is "viridis"
  if ( input[["expression_projection_color_scale"]] == 'viridis' ) {

    ## add color scale to plot
    plot <- plot +
      viridis::scale_fill_viridis(
        option = "viridis",
        limits = input[["expression_projection_color_scale_range"]],
        oob = scales::squish,
        direction = -1,
        name = "Log-normalised\nexpression",
        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
      )

  ## ... selected color scale is anything else than "viridis"
  } else {

    ## add color scale to plot
    plot <- plot +
      scale_fill_distiller(
        palette = input[["expression_projection_color_scale"]],
        limits = input[["expression_projection_color_scale_range"]],
        oob = scales::squish,
        direction = 1,
        name = "Log-normalised\nexpression",
        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
      )
  }

  ##
  return(plot)
}

##----------------------------------------------------------------------------##
## UI element with layout for user input and plot.
##----------------------------------------------------------------------------##

output[["expression_projection_UI"]] <- renderUI({
  fluidRow(
    column(
      width = 3, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = tagList(
          "Input parameters",
          actionButton(
            inputId = "expression_projection_parameters_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-right: 5px"
          )
        ),
        tagList(
          shinyWidgets::radioGroupButtons(
             inputId = "expression_analysis_mode",
             label = NULL,
             choices = c("Gene(s)", "Gene set"),
             status = "primary",
             justified = TRUE,
             width = "100%"
          ),
          uiOutput("expression_projection_input_type_UI"),
          uiOutput("expression_projection_parameters_UI"),
          uiOutput("expression_projection_color_scale_range_UI"),
          uiOutput("expression_projection_scales_UI")
        )
      )
    ),
    column(
      width = 9, offset = 0, style = "padding: 0px;",
      cerebroBox(
        title = tagList(
          boxTitle("Dimensional reduction"),
          tagList(
            actionButton(
              inputId = "expression_projection_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-right: 5px"
            ),
            shinyFiles::shinySaveButton(
              "expression_projection_export",
              label = "export to PDF",
              title = "Export dimensional reduction to PDF file.",
              filetype = "pdf",
              viewtype = "icon",
              class = "btn-xs",
              style = "margin-right: 5px"
            ),
            shinyWidgets::dropdownButton(
              tags$div(
                tags$style(
                  HTML("div.awesome-checkbox {margin-top: 10px;}")
                ),
                style = "color: black !important;",
                tagList(
                  ## TODO: figure out how to vertically center box and label
                  shinyWidgets::awesomeCheckbox(
                    inputId = "expression_projection_show_genes_in_separate_panels",
                    label = HTML("Show genes in separate panels<br>(experimental)"),
                    value = FALSE
                  )
                )
              ),
              circle = FALSE,
              icon = icon("cog"),
              inline = TRUE,
              size = "xs"
            )
          )
        ),
        tagList(
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              "expression_projection",
              width = "auto",
              height = "85vh"
            ),
            type = 8,
            hide.ui = FALSE
          ),
          tags$br(),
          htmlOutput("expression_number_of_selected_cells"),
          tags$br(),
          htmlOutput("expression_genes_displayed")
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI elements to choose whether gene(s) or gene sets should be analyzed
##----------------------------------------------------------------------------##

output[["expression_projection_input_type_UI"]] <- renderUI({

  req(
    input[["expression_analysis_mode"]]
  )

  if ( input[["expression_analysis_mode"]] == "Gene(s)" ) {
    selectizeInput(
      'expression_genes_input',
      label = 'Gene(s)',
      choices = data.table::as.data.table(data.frame("Genes" = getGeneNames())),
      multiple = TRUE,
      options = list(create = TRUE)
    )
  } else if ( input[["expression_analysis_mode"]] == "Gene set" ) {
    selectizeInput(
      'expression_select_gene_set',
      label = 'Gene set',
      choices = data.table::as.data.table(data.frame("Gene sets" = c("-", msigdbr:::msigdbr_genesets$gs_name))),
      multiple = FALSE
    )
  }
})

##----------------------------------------------------------------------------##
## UI elements to collect parameters for plot from user.
##----------------------------------------------------------------------------##

output[["expression_projection_parameters_UI"]] <- renderUI({

  ## get available projections
  available_projections <- availableProjections()

  ## collect available trajectories across all methods and create selectable
  ## options
  available_trajectories <- c()
  available_trajectory_method <- getMethodsForTrajectories()

  ## check if at least 1 trajectory method exists
  if ( length(available_trajectory_method) > 0 ) {

    ## cycle through trajectory methods
    for ( i in seq_along(available_trajectory_method) ) {

      ## get current method and names of trajectories for this method
      current_method <- available_trajectory_method[i]
      available_trajectories_for_this_method <- getNamesOfTrajectories(current_method)

      ## check if at least 1 trajectory is available for this method
      if ( length(available_trajectories_for_this_method) > 0 ) {

        ## cycle through trajectories for this method
        for ( j in seq_along(available_trajectories_for_this_method) ) {

          ## create selectable combination of method and trajectory name and add
          ## it to the available trajectories
          current_trajectory <- available_trajectories_for_this_method[j]
          available_trajectories <- c(
            available_trajectories,
            glue::glue("{current_method} // {current_trajectory}")
          )
        }
      }
    }
  }

  tagList(
    selectInput(
      "expression_projection_to_display",
      label = "Projection",
      choices = list(
        "Projections" = available_projections,
        "Trajectories" = available_trajectories
      )
    ),
    sliderInput(
      "expression_percentage_cells_to_show",
      label = "Show % of cells",
      min = scatter_plot_percentage_cells_to_show[["min"]],
      max = scatter_plot_percentage_cells_to_show[["max"]],
      step = scatter_plot_percentage_cells_to_show[["step"]],
      value = scatter_plot_percentage_cells_to_show[["default"]]
    ),
    selectInput(
      "expression_projection_plotting_order",
      label = "Plotting order",
      choices = c("Random", "Highest expression on top"),
      selected = "Random"
    ),
    sliderInput(
      "expression_projection_point_size",
      label = "Point size",
      min = scatter_plot_point_size[["min"]],
      max = scatter_plot_point_size[["max"]],
      step = scatter_plot_point_size[["step"]],
      value = scatter_plot_point_size[["default"]]
    ),
    sliderInput(
      "expression_projection_point_opacity",
      label = "Point opacity",
      min = scatter_plot_point_opacity[["min"]],
      max = scatter_plot_point_opacity[["max"]],
      step = scatter_plot_point_opacity[["step"]],
      value = scatter_plot_point_opacity[["default"]]
    ),
    hr(),
    selectInput(
      "expression_projection_color_scale",
      label = "Color scale",
      choices = c("YlGnBu", "YlOrRd","Blues","Greens","Reds","RdBu","viridis"),
      selected = "YlGnBu"
    )
  )
})

##----------------------------------------------------------------------------##
## UI element for color scale range in projection. This is a separate element
## because it requires generation of "gene_expression_plot_data()", which needs
## user input from other UI elements.
##----------------------------------------------------------------------------##

output[["expression_projection_color_scale_range_UI"]] <- renderUI({

  ## get range of expression levels
  expression_range <- range(gene_expression_plot_data()$level)

  ## adjust expression range for color scale
  ## ... there is no range (from 0 to 0)
  if (
    expression_range[1] == 0 &&
    expression_range[2] == 0
  ) {

    ## set range to 0-1
    expression_range[2] = 1

  ## ... otherwise
  } else {

    ## round min and max values to 2 digits
    expression_range <- round(expression_range, digits = 2)
  }

  tagList(
    sliderInput(
      "expression_projection_color_scale_range",
      label = "Range of color scale",
      min = expression_range[1],
      max = expression_range[2],
      value = expression_range
    )
  )
})

##----------------------------------------------------------------------------##
## UI elements to set X and Y scales in plot. Separate element because it
## requires user input from other UI elements.
##----------------------------------------------------------------------------##
output[["expression_projection_scales_UI"]] <- renderUI({

  ## don't proceed without these inputs
  req(
    input[["expression_projection_to_display"]]
  )

  ## check if projection or trajectory should be shown
  ## ... projection
  if ( input[["expression_projection_to_display"]] %in% availableProjections() ) {

    ##
    XYranges <- getXYranges(getProjection(input[["expression_projection_to_display"]]))

  ## ... trajectory
  } else {

    ## split selection into method and name
    selection <- strsplit(input[["expression_projection_to_display"]], split = ' // ')[[1]]

    ## check if method and name exist and don't proceed if not
    req(
      selection[1] %in% getMethodsForTrajectories(),
      selection[2] %in% getNamesOfTrajectories(selection[1])
    )

    ## collect trajectory data
    trajectory_data <- getTrajectory(
      selection[1],
      selection[2]
    )

    ##
    XYranges <- getXYranges(trajectory_data[["meta"]])
  }

  tagList(
    hr(),
    sliderInput(
      "expression_projection_scale_x_manual_range",
      label = "Range of X axis",
      min = XYranges$x$min,
      max = XYranges$x$max,
      value = c(XYranges$x$min, XYranges$x$max)
    ),
    sliderInput(
      "expression_projection_scale_y_manual_range",
      label = "Range of Y axis",
      min = XYranges$y$min,
      max = XYranges$y$max,
      value = c(XYranges$y$min, XYranges$y$max)
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["expression_projection_parameters_info"]], {
  showModal(
    modalDialog(
      expression_projection_parameters_info$text,
      title = expression_projection_parameters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

expression_projection_parameters_info <- list(
  title = "Parameters for gene (set) expression",
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Gene(s) / Gene set:</b> Select whether you would like to select individual genes or gene sets. In the case of 'Gene(s)', you can select one or multiple genes from the input field below. If you select multiple genes, the mean expression across the selected genes will be calculated for each cell. If you select 'Gene set', you can select a gene set from the MSigDB. Species-specific gene names will be tried to retrieve, otherwise gene name matching is attempted. A list of which genes are present or missing in the data set can be found below the projection.</li>
      <li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
      <li><b>Plotting order:</b> Cells can be plotted in random order or so that cells with highest expression are on top.</li>
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Color scale:</b> Choose your prefered color scale.</li>
      <li><b>Range of color scale:</b> Using the sliders, you can set the limits for the color scale. Values outside the scale will be shown in the color corresponding to the min/max value, respectively.</li>
      <li><b>Range of X/Y axis:</b> Set the X/Y axis limits. This is useful when you want to change the aspect ratio of the plot.</li>
    </ul>
    "
  )
)

##----------------------------------------------------------------------------##
## Function to get genes for selected gene set.
##----------------------------------------------------------------------------##

## get genes for gene set
getGenesForGeneSet <- function(gene_set) {

  if (
    !is.null(getExperiment()$organism) &&
    getExperiment()$organism == "mm"
  ) {
    species <- "Mus musculus"
  } else if (
    !is.null(getExperiment()$organism) &&
    getExperiment()$organism == "hg"
  ) {
    species <- "Homo sapiens"
  } else {
    species <- "Mus musculus"
  }

  ## - get list of gene set names
  ## - filter for selected gene set
  ## - extract genes that belong to the gene set
  ## - get orthologs for the genes
  ## - convert gene symbols to vector
  ## - only keep unique gene symbols
  ## - sort genes
  msigdbr:::msigdbr_genesets %>%
  dplyr::filter(.data$gs_name == gene_set) %>%
  dplyr::inner_join(
    .,
    msigdbr:::msigdbr_genes,
    by = "gs_id"
  ) %>%
  dplyr::inner_join(
    .,
    msigdbr:::msigdbr_orthologs %>%
      dplyr::filter(., .data$species_name == species),
    by = "human_entrez_gene"
  ) %>%
  dplyr::pull(gene_symbol) %>%
  unique() %>%
  sort()

}

##----------------------------------------------------------------------------##
## Plot of projection.
##----------------------------------------------------------------------------##

output[["expression_projection"]] <- plotly::renderPlotly({

  ## don't proceed without these inputs
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_point_size"]],
    input[["expression_projection_point_opacity"]],
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_scale_range"]],
    input[["expression_projection_scale_x_manual_range"]],
    input[["expression_projection_scale_y_manual_range"]],
    gene_expression_plot_data()
  )

  ## check selected color scale
  ## ... selected color scale is "viridis"
  if ( input[["expression_projection_color_scale"]] == 'viridis' ) {
    color_scale <- 'Viridis'

  ## ... selected color scale is anything else than "viridis"
  } else {
    color_scale <- input[["expression_projection_color_scale"]]
  }

  ## check if projection or trajectory should be shown
  ## ... projection
  if ( input[["expression_projection_to_display"]] %in% availableProjections() ) {

    ## check if user requested to show expression in separate panels
    ## ... separate panels requested, two-dimensional projection selected, and
    ##     "gene" column present
    if (
      ncol(getProjection(input[["expression_projection_to_display"]])) == 2 &&
      input[["expression_projection_show_genes_in_separate_panels"]] == TRUE &&
      "gene" %in% colnames(gene_expression_plot_data()) == TRUE
    ) {

      ## prepare plot
      plot <- plotExpressionMultiplePanels(gene_expression_plot_data())

      ## convert ggplot to plotly
      plot <- plotly::ggplotly(plot)

    ## ... if conditions for multiple panels are not met
    } else {

      ## prepare hover info
      hover_info <- buildHoverInfoForProjections(gene_expression_plot_data())

      ## add expression levels to hover info
      hover_info <- glue::glue(
        "{hover_info}
        <b>Expression level</b>: {formatC(gene_expression_plot_data()$level, format = 'f', big.mark = ',', digits = 3)}"
      )

      ## check if selection projection consists of 2 or 3 dimensions
      ## ... selection projection consists of 3 dimensions
      if ( ncol(getProjection(input[["expression_projection_to_display"]])) == 3 ) {

        ## prepare plot
        plot <- plotExpressionSinglePanel3D(gene_expression_plot_data(), color_scale, hover_info)

      ## ... selection projection consists of 2 dimensions
      } else if ( ncol(getProjection(input[["expression_projection_to_display"]])) == 2 ) {

        ## prepare plot
        plot <- plotExpressionSinglePanel2D(gene_expression_plot_data(), color_scale, hover_info)
      }
    }

  ## ... trajectory
  } else {

    ## split selection into method and name
    selection <- strsplit(input[["expression_projection_to_display"]], split = ' // ')[[1]]

    req(
      selection[1] %in% getMethodsForTrajectories(),
      selection[2] %in% getNamesOfTrajectories(selection[1])
    )

    ## collect trajectory data
    trajectory_data <- getTrajectory(
        selection[1],
        selection[2]
      )

    ## prepare hover info
    hover_info <- buildHoverInfoForProjections(gene_expression_plot_data())

    ## add expression levels to hover info
    hover_info <- glue::glue(
      "{hover_info}
      <b>Expression level</b>: {formatC(gene_expression_plot_data()$level, format = 'f', big.mark = ',', digits = 3)}"
    )

    plot <- plotExpressionSinglePanel2DTrajectory(
        gene_expression_plot_data(),
        trajectory_data[["edges"]],
        color_scale,
        hover_info
      )
  }

  ## return plot either with WebGL or without, depending on setting
  if ( preferences$use_webgl == TRUE ) {
    plot %>% plotly::toWebGL()
  } else {
    plot
  }
})

##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##

output[["expression_number_of_selected_cells"]] <- renderText({

  ## don't proceed without these inputs
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_point_size"]],
    input[["expression_projection_point_opacity"]],
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_scale_range"]],
    input[["expression_projection_scale_x_manual_range"]],
    input[["expression_projection_scale_y_manual_range"]],
    gene_expression_plot_data()
  )

  ## check selection
  ## ... selection has not been made or there is not cell in it
  if (
    is.null(plotly::event_data("plotly_selected", source = "expression_projection")) ||
    length(plotly::event_data("plotly_selected", source = "expression_projection")) == 0
  ) {

    ## manually set counter to 0
    number_of_selected_cells <- 0

  ## ... selection has been made and at least 1 cell is in it
  } else {

    ## get number of selected cells
    number_of_selected_cells <- nrow(plotly::event_data("plotly_selected", source = "expression_projection"))
  }

  ## prepare string to show
  paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
})

##----------------------------------------------------------------------------##
## Text showing which genes are present and missing.
##----------------------------------------------------------------------------##

output[["expression_genes_displayed"]] <- renderText({

  ## don't proceed without these inputs
  req(
    genesToPlot()
  )

  ## prepare text output from reactive data
  paste0(
    "<b>Showing expression for ",
    length(genesToPlot()[["genes_to_display_present"]]), " gene(s):</b><br>",
    paste0(genesToPlot()[["genes_to_display_present"]], collapse = ", "),
    "<br><br><b>",
    length(genesToPlot()[["genes_to_display_missing"]]),
    " gene(s) are not in data set: </b><br>",
    paste0(genesToPlot()[["genes_to_display_missing"]], collapse = ", ")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["expression_projection_info"]], {
  showModal(
    modalDialog(
      expression_projection_info$text,
      title = expression_projection_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

expression_projection_info <- list(
  title = "Dimensional reduction",
  text = HTML("
    Interactive projection of cells into two- or three-dimensional space based on their expression profile.<br>
    <ul>
      <li>Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44).</li>
      <li>Cell color reflects the log-normalised expression of entered genes. If more than 1 gene is entered or a gene set is selected, the color reflects the average expression of all genes. Genes must be in separate lines or separated by a space, comma, or semicolon. Reported below the projection are the genes that are present and absent in this data set. Absent genes could either have been annotated with a different name or were not expressed in any of the cells. Matching of gene names is case-insensitive, that means Myc/MYC/myc are treated equally.</li>
      <li>Cells can be plotted either randomly (to give a more unbiased perspective) or in the order of expression (with highest expression plotted last), sometimes resulting in a more appealing figure.</li>
      <li>The last two slider elements on the left can be used to resize the projection axes. This can be particularly useful when a projection contains a population of cell that is very far away from the rest and therefore creates a big empty space (which is not uncommon for UMAPs).</li>
    </ul>
    The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow.<br>
    <h4>Experimental options</h4>
    Experimental options can be accessed from the gear icon next to the 'export to PDF' button.<br>
    <b>Show genes in separate panels</b><br>
    When selecting multiple genes as input (up to 8), this option will show the expression of each gene in a separate panel instead of calculating the mean expression across all genes. The option is labeled as 'experimental' because of its poor implementation:
    <ol>
      <li>Hovering over the cells shows only limited information.</li>
      <li>Cells cannot be shown with a (grey) border around them.</li>
      <li>All genes have the some color scale.</li>
      <li>Cells cannot be selected.</li>
      <li>The 'Expression by group' panel needs to be deactivated.</li>
      <li>It throws annoying but innocent warning messages in the log.</li>
    </ol>
    Yet, this feature might be useful in some situations.
    "
  )
)

##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##

observeEvent(input[["expression_projection_export"]], {

  ## don't proceed without these inputs
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_plotting_order"]],
    input[["expression_projection_point_size"]],
    input[["expression_projection_point_opacity"]],
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_scale_range"]],
    input[["expression_projection_scale_x_manual_range"]],
    input[["expression_projection_scale_y_manual_range"]]
  )

  ## open dialog to select where plot should be saved and how the file should
  ## be named
  shinyFiles::shinyFileSave(
    input,
    id = "expression_projection_export",
    roots = available_storage_volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  ## retrieve info from dialog
  save_file_input <- shinyFiles::parseSavePath(available_storage_volumes, input[["expression_projection_export"]])

  ## only proceed if a path has been provided
  req(
    nrow(save_file_input) > 0
  )

  ## extract specified file path
  save_file_path <- as.character(save_file_input$datapath[1])

  ## check if projection or trajectory should be shown
  ## ... projection
  if ( input[["expression_projection_to_display"]] %in% availableProjections() ) {

    ## check if selection projection consists of 2 or 3 dimensions
    ## ... selection projection consists of 3 dimensions
    if ( ncol(getProjection(input[["expression_projection_to_display"]])) == 3 ) {

      ## give error message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Sorry!",
        text = "It's currently not possible to create PDF plots from 3D dimensional reductions. Please use the PNG export button in the panel or a 2D dimensional reduction instead.",
        type = "error"
      )

    ## ... selection projection consists of 2 dimensions
    } else if ( ncol(getProjection(input[["expression_projection_to_display"]])) == 2 ) {

      ## ... separate panels requested and "gene" column present
      if (
        input[["expression_projection_show_genes_in_separate_panels"]] == TRUE &&
        "gene" %in% colnames(gene_expression_plot_data()) == TRUE
      ) {

        ## prepare plot
        plot <- plotExpressionMultiplePanels(gene_expression_plot_data())

      ## ...
      } else {

        ## prepare plot
        plot <- plotExpressionSinglePanel2DExport(gene_expression_plot_data())
      }
    }

  ## ... trajectory
  } else {

    ## split selection into method and name
    selection <- strsplit(input[["expression_projection_to_display"]], split = ' // ')[[1]]

    req(
      selection[1] %in% getMethodsForTrajectories(),
      selection[2] %in% getNamesOfTrajectories(selection[1])
    )

    ## collect trajectory data
    trajectory_data <- getTrajectory(
      selection[1],
      selection[2]
    )

    ## prepare plot
    plot <- plotExpressionSinglePanel2DTrajectoryExport(
      gene_expression_plot_data(),
      trajectory_data[["edges"]]
    )
  }

  ## plot must be a ggplot object, otherwise don't proceed
  req(
    is.ggplot(plot)
  )

  ## save plot
  pdf(NULL)
  ggsave(save_file_path, plot, height = 8, width = 11)

  ## check if file was succesfully saved
  ## ... successful
  if ( file.exists(save_file_path) ) {

    ## give positive message
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Success!",
      text = paste0("Plot saved successfully as: ", save_file_path),
      type = "success"
    )

  ## ... failed
  } else {

    ## give negative message
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error!",
      text = "Sorry, it seems something went wrong...",
      type = "error"
    )
  }
})
