##----------------------------------------------------------------------------##
##
##----------------------------------------------------------------------------##
server <- function(input, output, session) {

  ##--------------------------------------------------------------------------##
  ## Color management.
  ##--------------------------------------------------------------------------##
  # Dutch palette from flatuicolors.com
  colorset_dutch <- c(
    "#FFC312","#C4E538","#12CBC4","#FDA7DF","#ED4C67",
    "#F79F1F","#A3CB38","#1289A7","#D980FA","#B53471",
    "#EE5A24","#009432","#0652DD","#9980FA","#833471",
    "#EA2027","#006266","#1B1464","#5758BB","#6F1E51"
  )

  # Spanish palette from flatuicolors.com
  colorset_spanish <- c(
    "#40407a","#706fd3","#f7f1e3","#34ace0","#33d9b2",
    "#2c2c54","#474787","#aaa69d","#227093","#218c74",
    "#ff5252","#ff793f","#d1ccc0","#ffb142","#ffda79",
    "#b33939","#cd6133","#84817a","#cc8e35","#ccae62"
  )

  default_colorset <- c(colorset_dutch, colorset_spanish)

  cell_cycle_colorset <- setNames(
    c("#45aaf2", "#f1c40f", "#e74c3c", "#7f8c8d"),
    c("G1",      "S",       "G2M",     "-")
  )

  ##--------------------------------------------------------------------------##
  ## Central parameters.
  ##--------------------------------------------------------------------------##
  scatter_plot_dot_size <- list(
    min = 1,
    max = 20,
    step = 1,
    default = 5
  )

  scatter_plot_dot_opacity <- list(
    min = 0.1,
    max = 1.0,
    step = 0.1,
    default = 1.0
  )

  scatter_plot_percentage_cells_to_show <- list(
    min = 10,
    max = 100,
    step = 10,
    default = 100
  )

  preferences <- reactiveValues(use_webgl = TRUE)

  ##--------------------------------------------------------------------------##
  ## Sidebar menu.
  ##--------------------------------------------------------------------------##
  output[["sidebar_menu"]] <- renderMenu({
    sidebarMenu(id = "sidebar",
      menuItem(
        "Load data", tabName = "loadData",
        icon = icon("spinner"), selected = TRUE
      ),
      menuItem(
        "Overview", tabName = "overview",
        icon = icon("binoculars")
      ),
      menuItem(
        "Groups", tabName = "groups",
        icon = icon("star")
      ),
      menuItem(
        "Most expressed genes", tabName = "mostExpressedGenes",
        icon = icon("bullhorn")
      ),
      menuItem(
        "Marker genes", tabName = "markerGenes",
        icon = icon("magnet")
      ),
      menuItem(
        "Enriched pathways", tabName = "enrichedPathways",
        icon = icon("sitemap")
      ),
      menuItem(
        "Gene expression", tabName = "geneExpression",
        icon = icon("signal")
      ),
      menuItem(
        "Gene set expression", tabName = "geneSetExpression",
        icon = icon("list")
      ),
      menuItem(
        "Trajectory", tabName = "trajectory",
        icon = icon("random")
      ),
      menuItem(
        "Gene ID conversion", tabName = "geneIdConversion",
        icon = icon("barcode")
      ),
      menuItem(
        "Analysis info", tabName = "info",
        icon = icon("info")
      ),
      menuItem(
        "Color management", tabName = "color_management",
        icon = icon("palette")
      ),
      menuItem(
        "About", tabName = "about",
        icon = icon("at")
      )
    )
  })

  ##--------------------------------------------------------------------------##
  ## Sample data.
  ##--------------------------------------------------------------------------##
  sample_data <- reactive({
    if ( is.null(input[["input_file"]]) || is.na(input[["input_file"]]) ) {
      sample_data <- readRDS(
        # system.file("extdata/v1.3/example.rds", package = "cerebroApp")
        ## TODO: change path
        "~/Dropbox/Cerebro_development/pbmc_Seurat.crb"
      )
    } else {
      req(input[["input_file"]])
      sample_data <- readRDS(input[["input_file"]]$datapath)
    }
    return(sample_data)
  })

  ##--------------------------------------------------------------------------##
  ## Functions to find columns of specific type (for automatic formatting).
  ##--------------------------------------------------------------------------##
  ## only test numeric columns
  findColumnsInteger <- function(df, columns_to_test) {
    columns_indices <- c()
    for ( i in columns_to_test ) {
      if (
        is.numeric(df[[i]]) &&
        all.equal(df[[i]], as.integer(df[[i]]), check.attributes = FALSE) == TRUE
      ) {
        columns_indices <- c(columns_indices, i)
      }
    }
    return(columns_indices)
  }

  findColumnsPercentage <- function(df) {
    columns_indices <- c()
    for ( i in 1:ncol(df) ) {
      if (
        grepl(colnames(df)[i], pattern = "pct|percent|%", ignore.case = TRUE) &&
        is.numeric(df[[i]]) &&
        min(df[[i]]) >= 0 &&
        max(df[[i]])
      ) {
        columns_indices <- c(columns_indices, i)
      }
    }
    return(columns_indices)
  }

  findColumnsPValues <- function(df) {
    pattern_columns_p_value <- "pval|p_val|p-val|p.val|padj|p_adj|p-adj|p.adj|adjp|adj_p|adj-p|adj.p|FDR|qval|q_val|q-val|q.val"
    columns_indices <- c()
    for ( i in 1:ncol(df) ) {
      if (
        grepl(colnames(df)[i], pattern = pattern_columns_p_value, ignore.case = TRUE) &&
        is.numeric(df[[i]]) &&
        min(df[[i]]) >= 0 &&
        max(df[[i]])
      ) {
        columns_indices <- c(columns_indices, i)
      }
    }
    return(columns_indices)
  }

  findColumnsLogFC <- function(df) {
    columns_indices <- c()
    for ( i in 1:ncol(df) ) {
      if (
        grepl(colnames(df)[i], pattern = "logFC|log-FC|log_FC|log.FC", ignore.case = TRUE) &&
        is.numeric(df[[i]])
      ) {
        columns_indices <- c(columns_indices, i)
      }
    }
    return(columns_indices)
  }

  ##--------------------------------------------------------------------------##
  ## Functions to prepare table.
  ##--------------------------------------------------------------------------##

  prettifyTable <- function(
    table,
    filter,
    dom,
    show_buttons = FALSE,
    number_formatting = FALSE,
    color_highlighting = FALSE,
    hide_long_columns = FALSE,
    columns_percentage = NULL,
    columns_hide = NULL,
    download_file_name = NULL,
    page_length_default = 15,
    page_length_menu = c(15, 30, 50, 100)
  ) {

    ## replace Inf and -Inf values in numeric columns with 999 or -999,
    ## respectively, because other the columns will be converted to characters
    ## which messes up sorting of values in that column
    table <- table %>%
      dplyr::mutate_if(is.numeric, function(x) ifelse(x == Inf, 999, x)) %>%
      dplyr::mutate_if(is.numeric, function(x) ifelse(x == -Inf, -999, x))

    ## get column type for alignment in table
    ## factors, characters and logical are centered and numeric columns are
    ## right-aligned
    columns_factor <- as.vector(which(unlist(lapply(table, is.factor))))
    columns_character <- as.vector(which(unlist(lapply(table, is.character))))
    columns_logical <- as.vector(which(unlist(lapply(table, is.logical))))
    columns_numeric <- as.vector(which(unlist(lapply(table, is.numeric))))

    ## identify columns which contain integer despite not being stored as
    ## integer type
    columns_integer <- findColumnsInteger(table, columns_numeric)

    ## identify which columns might contain percentages, p-values, and logFC
    columns_percent <- findColumnsPercentage(table)
    columns_p_value <- findColumnsPValues(table)
    columns_logFC <- findColumnsLogFC(table)

    ## find columns with very long (character) content so that they can be
    ## hidden
    columns_with_long_content <- c()
    if ( hide_long_columns == TRUE ) {
      for ( i in columns_character ) {
        if ( max(stringr::str_length(table[[i]])) > 200 ) {
          columns_with_long_content <- c(columns_with_long_content, i)
        }
      }
      ## reduce column indices by 1 because DT works with 0-based indices
      columns_with_long_content <- columns_with_long_content - 1
    }

    ## add manually specified column types
    if ( is.null(columns_percentage) == FALSE ) {
      columns_percent <- c(columns_percent, columns_percentage)
    }

    ## add manually specified columns to hide
    if ( is.null(columns_hide) == FALSE ) {
      columns_hide <- columns_hide - 1
    } else {
      columns_hide <- c()
    }

    ## remove columns with p-values from numeric columns to avoid applying color
    ## tiles
    columns_numeric <- columns_numeric[ columns_numeric %in% columns_p_value == FALSE ]

    ## get vector of column indices that contain numeric values which are
    ## neither integer, p-values, percentages, or logFC
    ## these columns will be rounded to significant digits (can only be done
    ## with DT so it won't be done with formattable)
    columns_only_numeric <- columns_numeric[ columns_numeric %in% c(
      columns_p_value, columns_percent, columns_integer, columns_p_value,
      columns_logFC) == FALSE ]

    ## add buttons if specified
    if ( show_buttons == TRUE ) {
      table_extensions <- c("Buttons", "ColReorder")
      table_buttons <- list(
        "colvis",
        list(
          extend = "collection",
          text = "Download",
          buttons = list(
            list(
              extend = "csv",
              filename = download_file_name,
              title = NULL
            ),
            list(
              extend = "excel",
              filename = download_file_name,
              title = NULL
            )
          )
        )
      )
    } else {
      table_extensions <- c("ColReorder")
      table_buttons <- list()
    }

    ## prepare table with formattable to use colors
    if ( color_highlighting == TRUE ) {

      ## format numbers (cannot be done with DT because the color formatting of
      ## formattable messes up number formatting with DT)
      if ( number_formatting == TRUE ) {

        ## remove digits behind comma for integer columns
        for ( i in columns_integer ) {
          table[[i]] <- formattable::comma(table[[i]], big.mark = ",", digits = 0)
        }

        ## show percentages properly
        for ( i in columns_percent ) {
          table[[i]] <- formattable::percent(table[[i]], digits = 2)
        }

        ## round logFC values
        for ( i in columns_logFC ) {
          table[[i]] <- formattable::digits(table[[i]], digits = 3)
        }

      }

      ## prepare color formatting
      format_list <- list()

      ## add color indicators for all numeric columns
      for ( i in columns_numeric ) {
        format_list[[ colnames(table)[i] ]] <- formattable::color_tile("white", "orange")
      }

      ## add color indicator for logical columns
      for ( i in columns_logical ) {
        format_list[[ colnames(table)[i] ]] <- formattable::formatter(
          "span",
          style = x~formattable::style(color = ifelse(x, "green", "red"))
        )
      }

      ## show percentages with pink color bar
      for ( i in columns_percent ) {
        format_list[[ colnames(table)[i] ]] <- formattable::color_bar("pink")
      }

      ## no colors for p-values because the range of values is often so large
      ## that it doesn't seem to help

      ## prepare table
      table <- formattable::formattable(
          table,
          format_list
        ) %>%
        formattable::as.datatable(
          autoHideNavigation = TRUE,
          class = "stripe table-bordered table-condensed",
          escape = FALSE,
          extensions = table_extensions,
          filter = filter,
          rownames = FALSE,
          selection = "single",
          style = "bootstrap",
          options = list(
            buttons = table_buttons,
            columnDefs = list(
              list(targets = "_all", className = 'dt-middle'),
              list(targets = c(columns_hide, columns_with_long_content), visible = FALSE)
            ),
            colReorder = list(
              realtime = FALSE
            ),
            dom = dom,
            lengthMenu = page_length_menu,
            pageLength = page_length_default,
            scrollX = TRUE
          )
        )

    ## if no color formatting is used, prepare table with DT which has the
    ## advantage that column filters provide extra functionality for numeric and
    ## factor columns
    } else {

      table <- DT::datatable(
        table,
        autoHideNavigation = TRUE,
        class = "stripe table-bordered table-condensed",
        escape = FALSE,
        extensions = table_extensions,
        filter = filter,
        rownames = FALSE,
        selection = "single",
        style = "bootstrap",
        options = list(
          buttons = table_buttons,
          columnDefs = list(
            list(targets = "_all", className = 'dt-middle'),
            list(targets = c(columns_hide, columns_with_long_content), visible = FALSE)
          ),
          colReorder = list(
            realtime = FALSE
          ),
          dom = dom,
          lengthMenu = page_length_menu,
          pageLength = page_length_default,
          scrollX = TRUE
        )
      )

    }

    ## align characters and numbers
    table <- table %>%
      DT::formatStyle(
        columns = c(columns_factor, columns_character, columns_logical),
        textAlign = 'center'
      ) %>%
      DT::formatStyle(
        columns = c(columns_numeric, columns_p_value),
        textAlign = 'right'
      )

    ## if automatic number formatting is on, apply formatting to columns which
    ## seem to contain p-values and logFC values
    if ( number_formatting == TRUE ) {
      if ( !is.null(columns_p_value) && length(columns_p_value) > 0 ) {
        table <- table %>%
          DT::formatSignif(
            columns = columns_p_value,
            digits = 3
          )
      }
      if ( !is.null(columns_logFC) && length(columns_logFC) > 0 ) {
        table <- table %>%
          DT::formatRound(
            columns = columns_logFC,
            digits = 3
          )
      }
      if ( !is.null(columns_only_numeric) && length(columns_only_numeric) > 0 ) {
        table <- table %>%
          DT::formatSignif(
            columns = columns_only_numeric,
            digits = 3
          )
      }
    }

    ## if automatic number formatting is on but color formatting is off, round
    ## integers and percentage values as formattable would do it
    if (
      number_formatting == TRUE &&
      color_highlighting == FALSE
    ) {
      if ( !is.null(columns_integer) && length(columns_integer) > 0 ) {
        table <- table %>%
          DT::formatRound(
            columns = columns_integer,
            digits = 0,
            interval = 3,
            mark = ","
          )
      }
      if ( !is.null(columns_percent) && length(columns_percent) > 0 ) {
        table <- table %>%
        DT::formatPercentage(
          columns = columns_percent,
          digits = 2
        )
      }
    }

    return(table)

  }

  prepareEmptyTable <- function(table) {
    DT::datatable(
      table,
      autoHideNavigation = TRUE,
      class = "stripe table-bordered table-condensed",
      escape = FALSE,
      filter = "none",
      rownames = FALSE,
      selection = "none",
      style = "bootstrap",
      options = list(
        buttons = list(),
        dom = "Brtip",
        lengthMenu = c(20, 50, 100),
        pageLength = 20,
        scrollX = TRUE
      )
    )
  }


  ##--------------------------------------------------------------------------##
  ## Function to calculate A-by-B tables (e.g. samples by clusters).
  ##--------------------------------------------------------------------------##
  calculateTableAB <- function(groupA, groupB) {
    ## check if specified group columns exist in meta data
    if ( groupA %in% colnames(colData(sample_data()$expression)) == FALSE ) {
      stop(
        paste0(
          "Column specified as groupA (`", groupA,
          "`) could not be found in meta data."
        ),
        call. = FALSE
      )
    }

    if ( groupB %in% colnames(colData(sample_data()$expression)) == FALSE ) {
      stop(
        paste0(
          "Column specified as groupB (`", groupB,
          "`) could not be found in meta data."
        ),
        call. = FALSE
      )
    }
    table <- colData(sample_data()$expression)[,c(groupA, groupB)] %>%
      as.data.frame()

    ## factorize group columns A and B if not already a factor
    if ( is.character(table[,groupA]) ) {
      table[[,groupA]] <- factor(
        table[[,groupA]],
        levels = table[[,groupA]] %>% unique() %>% sort()
      )
    }
    if ( is.character(table[,groupB]) ) {
      table[[,groupB]] <- factor(
        table[[,groupB]],
        levels = table[[,groupB]] %>% unique() %>% sort()
      )
    }

    ## generate table
    table <- table %>%
      dplyr::group_by_at(c(groupA, groupB)) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      tidyr::spread(`groupB`, count, fill = 0) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
      dplyr::select(c(`groupA`, 'total_cell_count', dplyr::everything()))

    ## fix order of columns if cell cycle info was chosen as second group
    if ( 'G1' %in% colnames(table) && 'G2M' %in% colnames(table) && 'S' %in% colnames(table) ) {
      table <- table %>%
        dplyr::select(c(`groupA`, 'total_cell_count', 'G1', 'S', 'G2M', dplyr::everything()))
    }

    ## return
    return(table)
  }

  ##--------------------------------------------------------------------------##
  ## Colors for groups.
  ##--------------------------------------------------------------------------##
  reactive_colors <- reactive({
    colors <- list()

    ## go through all groups
    for ( group_name in sample_data()$getGroups() )
    {
      ## if color selection from the "Color management" tab exist, assign those
      ## colors, otherwise assign colors from default colorset
      if ( !is.null(input[[ paste0('color_', group_name, '_', sample_data()$getGroupLevels(group_name)[1]) ]]) )
      {
        for ( group_level in sample_data()$getGroupLevels(group_name) )
        {
          ## it seems that special characters are not handled well in input/output
          ## so I replace them with underscores using gsub()
          colors[[ group_name ]][ group_level ] <- input[[ paste0('color_', group_name, '_', gsub(group_level, pattern = '[^[:alnum:]]', replacement = '_')) ]]
        }
      } else
      {
        colors[[ group_name ]] <- default_colorset[1:length(sample_data()$getGroupLevels(group_name))]
        names(colors[[ group_name ]]) <- sample_data()$getGroupLevels(group_name)
      }
    }

    ## go through columns with cell cycle info
    if ( length(sample_data()$cell_cycle) > 0 )
    {
      for ( column in sample_data()$cell_cycle )
      {
        ## if color selection from the "Color management" tab exist, assign those
        ## colors, otherwise assign colors from cell cycle colorset
        if ( !is.null(input[[ paste0('color_', column, '_', unique(as.character(colData(sample_data()$expression)[[ column ]]))[1]) ]]) )
        {
          for ( state in unique(as.character(colData(sample_data()$expression)[[ column ]])) )
          {
            ## it seems that special characters are not handled well in input/output
            ## so I replace them with underscores using gsub()
            colors[[ column ]][ state ] <- input[[ paste0('color_', column, '_', gsub(state, pattern = '[^[:alnum:]]', replacement = '_')) ]]
          }
        } else
        {
          colors[[ column ]] <- cell_cycle_colorset[1:length(unique(as.character(colData(sample_data()$expression)[[ column ]])))]
          names(colors[[ column ]]) <- unique(as.character(colData(sample_data()$expression)[[ column ]]))
        }
      }
    }

    return(colors)
  })

  ## TODO: add description
  volumes <- c(Home = "~", getVolumes()())

  ##--------------------------------------------------------------------------##
  ## Tabs.
  ##--------------------------------------------------------------------------##
  ## TODO: change path
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/load_data/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/overview/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/groups/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/most_expressed_genes/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/marker_genes/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/enriched_pathways/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/gene_expression/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/gene_set_expression/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/gene_id_conversion/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/trajectory/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/analysis_info/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/color_management/server.R", local = TRUE)
  source("~/Research/GitHub/cerebroApp_v1.3/inst/shiny/v1.3/about/server.R", local = TRUE)

}
