##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##
## Expression in projection.
##----------------------------------------------------------------------------##

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
    tagList(
      selectizeInput(
        'expression_genes_input',
        label = 'Gene(s)',
        choices = data.table::as.data.table(data.frame("Genes" = getGeneNames())),
        multiple = TRUE
      )
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
  tagList(
    selectInput(
      "expression_projection_to_display",
      label = "Projection",
      choices = availableProjections()
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
  if ( expression_range[1] == 0 & expression_range[2] == 0 ) {

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

  ##
  projection_to_display <- input[["expression_projection_to_display"]]
  range_x_min <- getProjection(projection_to_display)[,1] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
  range_x_max <- getProjection(projection_to_display)[,1] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
  range_y_min <- getProjection(projection_to_display)[,2] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
  range_y_max <- getProjection(projection_to_display)[,2] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
  tagList(
    sliderInput(
      "expression_projection_scale_x_manual_range",
      label = "Range of X axis",
      min = range_x_min,
      max = range_x_max,
      value = c(range_x_min, range_x_max)
    ),
    sliderInput(
      "expression_projection_scale_y_manual_range",
      label = "Range of Y axis",
      min = range_y_min,
      max = range_y_max,
      value = c(range_y_min, range_y_max)
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
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

## TODO: update text (only input, not plot)
expression_projection_info <- list(
  title = "Dimensional reduction",
  text = p(
    "Interactive projection of cells into 2-dimensional space based on their expression profile.",
    tags$ul(
      tags$li("Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44)."),
      tags$li("Cell color reflects the log-normalised expression of entered genes. If more than 1 gene is entered, the color reflects the average expression of all genes. Genes must be in separate lines or separated by a space, comma, or semicolon. Reported below the projection are the genes that are present and absent in this data set. Absent genes could either have been annotated with a different name or were not expressed in any of the cells. Matching of gene names is case-insensitive, that means Myc/MYC/myc are treated equally."),
      tags$li("Samples and clusters can be removed from the plot individually to highlight a contrast of interest."),
      tags$li("Cells can be plotted either randomly (which a more unbiased image) or in the order of expression (with highest expression plotted last), sometimes resulting in a more appealing figure."),
      tags$li("By default, the point size is set to 15 without any transparency but both these attributes can be changed using the sliders on the left."),
      tags$li("The last 2 slider elements on the left can be used to resize the projection axes. This can be particularly useful when a projection contains a population of cell that is very far away from the rest and therefore creates a big empty space (which is not uncommon for UMAPs).")
    ),
    "The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow."
  )
)

##----------------------------------------------------------------------------##
## Function to get genes for selected gene set.
##----------------------------------------------------------------------------##

## TODO: prepare for scenario where no genes are available for the given species

## get genes for gene set
getGenesForGeneSet <- function(gene_set) {

  if ( getExperiment()$organism == "mm" ) {
    species <- "Mus musculus"
  } else if ( getExperiment()$organism == "hg" ) {
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
  filter(.data$gs_name == gene_set) %>%
  inner_join(
    .,
    msigdbr:::msigdbr_genes,
    by = "gs_id"
  ) %>%
  inner_join(
    .,
    msigdbr:::msigdbr_orthologs %>%
      filter(., .data$species_name == species),
    by = "human_entrez_gene"
  ) %>%
  pull(gene_symbol) %>%
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

  ## prepare tooltip/hover info
  tooltip_info <- paste0(
    "<b>Cell</b>: ", gene_expression_plot_data()[[ "cell_barcode" ]], "<br>",
    "<b>Transcripts</b>: ", formatC(gene_expression_plot_data()[[ "nUMI" ]], format = "f", big.mark = ",", digits = 0), "<br>",
    "<b>Expressed genes</b>: ", formatC(gene_expression_plot_data()[[ "nGene" ]], format = "f", big.mark = ",", digits = 0), "<br>",
    "<b>Expression level</b>: ", formatC(gene_expression_plot_data()$level, format = "f", big.mark = ",", digits = 3), "<br>"
  )

  ## add info for known grouping variables to tooltip/hover
  for ( group in getGroups() ) {
    tooltip_info <- paste0(
      tooltip_info,
      "<b>", group, "</b>: ", gene_expression_plot_data()[[ group ]], "<br>"
    )
  }

  ## check if selection projection consists of 2 or 3 dimensions
  ## ... selection projection consists of 3 dimensions
  if ( ncol(getProjection(input[["expression_projection_to_display"]])) == 3 ) {

    ## prepare plot
    plotly::plot_ly(
      gene_expression_plot_data(),
      x = gene_expression_plot_data()[,1],
      y = gene_expression_plot_data()[,2],
      z = gene_expression_plot_data()[,3],
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
      text = ~tooltip_info,
      source = "expression_projection"
    ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(
          title = colnames(gene_expression_plot_data())[1],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = colnames(gene_expression_plot_data())[2],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        zaxis = list(
          title = colnames(gene_expression_plot_data())[3],
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

  ## ... selection projection consists of 2 dimensions
  } else if ( ncol(getProjection(input[["expression_projection_to_display"]])) == 2 ) {

    ## prepare plot
    plot <- plotly::plot_ly(
        gene_expression_plot_data(),
        x = gene_expression_plot_data()[,1],
        y = gene_expression_plot_data()[,2],
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
        text = ~tooltip_info,
        source = "expression_projection"
      ) %>%
      plotly::layout(
        xaxis = list(
          title = colnames(gene_expression_plot_data())[1],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = c(
            input[["expression_projection_scale_x_manual_range"]][1],
            input[["expression_projection_scale_x_manual_range"]][2]
          )
        ),
        yaxis = list(
          title = colnames(gene_expression_plot_data())[2],
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

    ## return plot either with WebGL or without, depending on setting
    if ( preferences$use_webgl == TRUE ) {
      plot %>% plotly::toWebGL()
    } else {
      plot
    }
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
    "<br><b>",
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
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

expression_projection_info <- list(
  title = "Dimensional reduction",
  text = p(
    "Interactive projection of cells into 2-dimensional space based on their expression profile.",
    tags$ul(
      tags$li("Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44)."),
      tags$li("Cell color reflects the log-normalised expression of entered genes. If more than 1 gene is entered, the color reflects the average expression of all genes. Genes must be in separate lines or separated by a space, comma, or semicolon. Reported below the projection are the genes that are present and absent in this data set. Absent genes could either have been annotated with a different name or were not expressed in any of the cells. Matching of gene names is case-insensitive, that means Myc/MYC/myc are treated equally."),
      tags$li("Samples and clusters can be removed from the plot individually to highlight a contrast of interest."),
      tags$li("Cells can be plotted either randomly (which a more unbiased image) or in the order of expression (with highest expression plotted last), sometimes resulting in a more appealing figure."),
      tags$li("By default, the point size is set to 15 without any transparency but both these attributes can be changed using the sliders on the left."),
      tags$li("The last 2 slider elements on the left can be used to resize the projection axes. This can be particularly useful when a projection contains a population of cell that is very far away from the rest and therefore creates a big empty space (which is not uncommon for UMAPs).")
    ),
    "The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow."
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
  shinyFileSave(
    input,
    id = "expression_projection_export",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  ## retrieve info from dialog
  fileinfo <- parseSavePath(volumes, input[["expression_projection_export"]])

  ## only proceed if a path has been provided
  if ( nrow(fileinfo) > 0 ) {

    ## extract specified file path
    file_output <- as.character(fileinfo$datapath[1])

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

      ## prepare plot
      p <- ggplot(
          gene_expression_plot_data(),
          aes_q(
            x = as.name(colnames(gene_expression_plot_data())[1]),
            y = as.name(colnames(gene_expression_plot_data())[2]),
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
          p <- p + viridis::scale_fill_viridis(
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
          p <- p + scale_fill_distiller(
            palette = input[["expression_projection_color_scale"]],
            limits = input[["expression_projection_color_scale_range"]],
            oob = scales::squish,
            direction = 1,
            name = "Log-normalised\nexpression",
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
          )
        }

      ## save plot
      pdf(NULL)
      ggsave(file_output, p, height = 8, width = 11)

      ## check if file was succesfully saved
      ## ... successful
      if ( file.exists(file_output) ) {

        ## give positive message
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Success!",
          text = paste0("Plot saved successfully as: ", file_output),
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
    }
  }
})
