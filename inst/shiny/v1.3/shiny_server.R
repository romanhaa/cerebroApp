##----------------------------------------------------------------------------##
## Server function for Shiny app.
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
  scatter_plot_point_size <- list(
    min = 1,
    max = 20,
    step = 1,
    default = 5
  )

  scatter_plot_point_opacity <- list(
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
        "Gene (set) expression", tabName = "geneExpression",
        icon = icon("signal")
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
  ## Load data set.
  ##--------------------------------------------------------------------------##

  data_set <- reactive({

    ## check what data to load
    ## ... a .crb file was specified to be loaded into Cerebro on launch and
    ##     the file exists
    if (
      !is.null(.GlobalEnv$Cerebro.options[["crb_file_to_load"]]) &&
      file.exists(.GlobalEnv$Cerebro.options[["crb_file_to_load"]])
    ) {

      ## load the specified file
      data <- readRDS(.GlobalEnv$Cerebro.options[["crb_file_to_load"]])

    ## ... no file was specified to be loaded
    } else if (
      is.null(input[["input_file"]]) ||
      is.na(input[["input_file"]])
    ) {

      ## load small example data set
      data <- readRDS(
        system.file("extdata/v1.3/example.rds", package = "cerebroApp")
      )

    ## ... none of the above
    } else {

      ## wait until input file was specified in "Load data" element
      req(
        input[["input_file"]]
      )

      ## load specified file
      data <- readRDS(input[["input_file"]]$datapath)
    }

    ## return loaded data
    return(data)
  })

  ##--------------------------------------------------------------------------##
  ## Functions to find columns of specific type (for automatic formatting).
  ##--------------------------------------------------------------------------##
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

    table_original <- table

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
    ## these columns will be rounded to significant digits
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

    ## - create table
    ## - prevent text wrap for characters/factors/logicals
    ## - align characters in left
    ## - align factors/logicals in center
    ## - align numerics to the right
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
      ) %>%
      DT::formatStyle(
        columns = c(columns_character),
        textAlign = 'left',
        "white-space" = "nowrap"
      ) %>%
      DT::formatStyle(
        columns = c(columns_factor, columns_logical),
        textAlign = 'center',
        "white-space" = "nowrap"
      ) %>%
      DT::formatStyle(
        columns = c(columns_numeric, columns_p_value),
        textAlign = 'right'
      )

    ## if automatic number formatting is on...
    ## - remove decimals from integers
    ## - show 3 significant decimals for p-values
    ## - show 3 decimals for logFC
    ## - show percentage values with percent symbol and 2 decimals
    ## - show all other numeric values that are none of the above with 3
    ##   significant decimals
    if ( number_formatting == TRUE ) {

      ## integer values
      if ( !is.null(columns_integer) && length(columns_integer) > 0 ) {
        table <- table %>%
          DT::formatRound(
            columns = columns_integer,
            digits = 0,
            interval = 3,
            mark = ","
          )
      }

      ## p-values
      if ( !is.null(columns_p_value) && length(columns_p_value) > 0 ) {
        table <- table %>%
          DT::formatSignif(
            columns = columns_p_value,
            digits = 3
          )
      }

      ## logFC
      if ( !is.null(columns_logFC) && length(columns_logFC) > 0 ) {
        table <- table %>%
          DT::formatRound(
            columns = columns_logFC,
            digits = 3
          )
      }

      ## percentage
      if ( !is.null(columns_percent) && length(columns_percent) > 0 ) {
        table <- table %>%
        DT::formatPercentage(
          columns = columns_percent,
          digits = 2
        )
      }

      ## numeric but none of the above
      if ( !is.null(columns_only_numeric) && length(columns_only_numeric) > 0 ) {
        table <- table %>%
          DT::formatSignif(
            columns = columns_only_numeric,
            digits = 3
          )
      }
    }

    ## if color highlighting is on...
    ## - use color bar for percentages
    ## - use color bar for p-values (ideally I wanted to use colors with
    ##   styleInterval(), but styleInterval() cannot handle missing cuts; I'd
    ##   like to color values based on fixed intervals, e.g. below 0.1, 0.05,
    ##   etc, but if the column doesn't contain any values for a cut, then the
    ##   function will results in an error)
    ## - use color scale for logFC
    ## - use color scale for integer
    ## - use color scale for numeric values that are none of the above
    ## - use colors for logicals
    ## - use reactive colors for grouping variables
    ## - use reactive colors for cell cycle assignments
    ## NOTES:
    ## - "styleInterval()" only works when there are at least two values that
    ##   are not the same, therefore a few tests are necessary to prevent errors
    if ( color_highlighting == TRUE ) {

      ## percentage
      if (
        !is.null(columns_percent) &&
        length(columns_percent) > 0
      ) {
        table <- table %>%
          DT::formatStyle(
            columns = columns_percent,
            background = DT::styleColorBar(c(0,1), 'pink'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      }

      ## p-values
      if (
        !is.null(columns_p_value) &&
        length(columns_p_value) > 0
      ) {
        table <- table %>%
          DT::formatStyle(
            columns = columns_p_value,
            background = DT::styleColorBar(c(1,0), '#e74c3c'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      }

      ## logFC
      if (
        !is.null(columns_logFC) &&
        length(columns_logFC) > 0 &&
        nrow(table_original) > 1
      ) {
        for ( i in columns_logFC ) {
          range <- range(table_original[[i]])
          if ( range[1] != range[2] ) {
            table <- table %>%
              DT::formatStyle(
                columns = i,
                backgroundColor = DT::styleInterval(
                  seq(range[1], range[2], (range[2]-range[1])/100),
                  colorRampPalette(colors = c('white', '#e67e22'))(102)
                )
              )
          }
        }
      }

      ## integer
      if (
        !is.null(columns_integer) &&
        length(columns_integer) > 0 &&
        nrow(table_original) > 1
      ) {
        for ( i in columns_integer ) {
          range <- range(table_original[[i]])
          if ( range[1] != range[2] ) {
            table <- table %>%
              DT::formatStyle(
                columns = i,
                backgroundColor = DT::styleInterval(
                  seq(range[1], range[2], (range[2]-range[1])/100),
                  colorRampPalette(colors = c('white', '#e67e22'))(102)
                )
              )
          }
        }
      }

      ## numeric values that are non of the above
      if (
        !is.null(columns_only_numeric) &&
        length(columns_only_numeric) > 0 &&
        nrow(table_original) > 1
      ) {
        for ( i in columns_only_numeric ) {
          range <- range(table_original[[i]])
          if ( range[1] != range[2] ) {
            table <- table %>%
              DT::formatStyle(
                columns = i,
                backgroundColor = DT::styleInterval(
                  seq(range[1], range[2], (range[2]-range[1])/100),
                  colorRampPalette(colors = c('white', '#e67e22'))(102)
                )
              )
          }
        }
      }

      ## logicals
      if ( !is.null(columns_logical) && length(columns_logical) > 0 ) {
        table <- table %>%
          DT::formatStyle(
            columns_logical,
            color = DT::styleEqual(c(TRUE, FALSE), c('#27ae60', '#e74c3c')),
            fontWeight = DT::styleEqual(c(TRUE, FALSE), c('bold', 'normal'))
          )
      }

      ## grouping variables
      columns_groups <- which(colnames(table_original) %in% getGroups())
      if ( length(columns_groups) > 0 ) {
        for ( i in columns_groups ) {
          group <- colnames(table_original)[i]
          if ( all(unique(table_original[[i]]) %in% names(reactive_colors()[[group]])) ) {
            table <- table %>%
              DT::formatStyle(
                i,
                backgroundColor = DT::styleEqual(
                  names(reactive_colors()[[group]]),
                  reactive_colors()[[group]]
                ),
                fontWeight = 'bold'
              )
          }
        }
      }

      ## cell cycle assignments
      columns_cell_cycle <- which(colnames(table_original) %in% getCellCycle())
      if ( length(columns_cell_cycle) > 0 ) {
        for ( i in columns_cell_cycle ) {
          method <- colnames(table_original)[i]
          if ( all(unique(table_original[[i]]) %in% names(reactive_colors()[[method]])) ) {
            table <- table %>%
              DT::formatStyle(
                i,
                backgroundColor = DT::styleEqual(
                  names(reactive_colors()[[method]]),
                  reactive_colors()[[method]]
                ),
                fontWeight = 'bold'
              )
          }
        }
      }
    }

    ## return the table
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
  ## Functions to interact with data set.
  ##
  ## Never directly interact with data set: data_set()
  ## This ensures that ...
  ##--------------------------------------------------------------------------##
  getExperiment <- function() {
    return(data_set()$getExperiment())
  }
  getParameters <- function() {
    return(data_set()$getParameters())
  }
  getTechnicalInfo <- function() {
    return(data_set()$getTechnicalInfo())
  }
  getGeneLists <- function() {
    return(data_set()$getGeneLists())
  }
  getExpression <- function() {
    return(data_set()$getExpression())
  }
  getCellIDs <- function() {
    return(colnames(data_set()$getExpression()))
  }
  getGeneNames <- function() {
    return(rownames(data_set()$getExpression()))
  }
  getGroups <- function() {
    return(data_set()$getGroups())
  }
  getGroupLevels <- function(group) {
    return(data_set()$getGroupLevels(group))
  }
  getCellCycle <- function() {
    return(data_set()$getCellCycle())
  }
  getMetaData <- function() {
    return(data_set()$getMetaData())
  }
  availableProjections <- function() {
    return(data_set()$availableProjections())
  }
  getProjection <- function(name) {
    return(data_set()$getProjection(name))
  }
  getTree <- function(group) {
    return(data_set()$getTree(group))
  }
  getGroupsWithMostExpressedGenes <- function() {
    return(data_set()$getGroupsWithMostExpressedGenes())
  }
  getMostExpressedGenes <- function(group) {
    return(data_set()$getMostExpressedGenes(group))
  }
  getMethodsForMarkerGenes <- function() {
    return(data_set()$getMethodsForMarkerGenes())
  }
  getGroupsWithMarkerGenes <- function(method) {
    return(data_set()$getGroupsWithMarkerGenes(method))
  }
  getMarkerGenes <- function(method, group) {
    return(data_set()$getMarkerGenes(method, group))
  }
  getMethodsForEnrichedPathways <- function() {
    return(data_set()$getMethodsForEnrichedPathways())
  }
  getGroupsWithEnrichedPathways <- function(method) {
    return(data_set()$getGroupsWithEnrichedPathways(method))
  }
  getEnrichedPathways <- function(method, group) {
    return(data_set()$getEnrichedPathways(method, group))
  }
  getMethodsForTrajectories <- function() {
    return(data_set()$getMethodsForTrajectories())
  }
  getNamesOfTrajectories <- function(method) {
    return(data_set()$getNamesOfTrajectories(method))
  }
  getTrajectory <- function(method, name) {
    return(data_set()$getTrajectory(method, name))
  }

  ##--------------------------------------------------------------------------##
  ## Function to calculate A-by-B tables, e.g. samples by clusters.
  ##--------------------------------------------------------------------------##

  calculateTableAB <- function(table, groupA, groupB) {

    ## check if specified group columns exist in table
    if ( groupA %in% colnames(table) == FALSE ) {
      stop(
        paste0(
          "Column specified as groupA (`", groupA,
          "`) could not be found in meta data."
        ),
        call. = FALSE
      )
    }

    if ( groupB %in% colnames(table) == FALSE ) {
      stop(
        paste0(
          "Column specified as groupB (`", groupB,
          "`) could not be found in meta data."
        ),
        call. = FALSE
      )
    }

    ## subset columns
    table <- table[,c(groupA, groupB)]

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
      dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
      tidyr::pivot_wider(
        id_cols = 1,
        names_from = tidyselect::all_of(groupB),
        values_from = "count",
        values_fill = 0
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(total_cell_count = rowSums(.[c(2:ncol(.))])) %>%
      dplyr::select(
        tidyselect::all_of(groupA), 'total_cell_count',
        dplyr::everything()
      )

    ## fix order of columns if cell cycle info was chosen as second group
    if (
      'G1' %in% colnames(table) &&
      'G2M' %in% colnames(table) &&
      'S' %in% colnames(table)
    ) {
      table <- table %>%
        dplyr::select(
          tidyselect::all_of(groupA), 'total_cell_count', 'G1', 'S', 'G2M',
          dplyr::everything()
        )
    }

    ## return
    return(table)
  }

  ##--------------------------------------------------------------------------##
  ## Colors for groups.
  ##--------------------------------------------------------------------------##

  reactive_colors <- reactive({

    ## get cell meta data
    meta_data <- getMetaData()

    colors <- list()

    ## go through all groups
    for ( group_name in getGroups() ) {
      ## if color selection from the "Color management" tab exist, assign those
      ## colors, otherwise assign colors from default colorset
      if ( !is.null(input[[ paste0('color_', group_name, '_', getGroupLevels(group_name)[1]) ]]) ) {
        for ( group_level in getGroupLevels(group_name) ) {
          ## it seems that special characters are not handled well in input/output
          ## so I replace them with underscores using gsub()
          colors[[ group_name ]][ group_level ] <- input[[ paste0('color_', group_name, '_', gsub(group_level, pattern = '[^[:alnum:]]', replacement = '_')) ]]
        }
      } else {
        colors[[ group_name ]] <- default_colorset[seq_along(getGroupLevels(group_name))]
        names(colors[[ group_name ]]) <- getGroupLevels(group_name)
      }
    }

    ## go through columns with cell cycle info
    if ( length(getCellCycle()) > 0 ) {
      for ( column in getCellCycle() ) {
        ## if color selection from the "Color management" tab exist, assign those
        ## colors, otherwise assign colors from cell cycle colorset
        if ( !is.null(input[[ paste0('color_', column, '_', unique(as.character(meta_data[[ column ]]))[1]) ]]) ) {
          for ( state in unique(as.character(meta_data[[ column ]])) ) {
            ## it seems that special characters are not handled well in input/output
            ## so I replace them with underscores using gsub()
            colors[[ column ]][ state ] <- input[[ paste0('color_', column, '_', gsub(state, pattern = '[^[:alnum:]]', replacement = '_')) ]]
          }
        } else {
          colors[[ column ]] <- cell_cycle_colorset[seq_along(unique(as.character(meta_data[[ column ]])))]
          names(colors[[ column ]]) <- unique(as.character(meta_data[[ column ]]))
        }
      }
    }

    return(colors)
  })

  ##--------------------------------------------------------------------------##
  ## Define and identify available volumes for saving plots.
  ##--------------------------------------------------------------------------##

  volumes <- c(
    Home = "~",
    shinyFiles::getVolumes()()
  )

  ##--------------------------------------------------------------------------##
  ## Assign colors to groups.
  ##--------------------------------------------------------------------------##

  assignColorsToGroups <- function(table, grouping_variable) {

    ## check if colors are already assigned in reactive_colors()
    ## ... already assigned
    if ( grouping_variable %in% names(reactive_colors()) ) {

      ## take colors from reactive_colors()
      colors_for_groups <- reactive_colors()[[ grouping_variable ]]

    ## ... not assigned but values are either factors or characters
    } else if (
      is.factor(table[[ grouping_variable ]]) ||
      is.character(table[[ grouping_variable ]])
    ) {

      ## check type of values
      ## ... factors
      if ( is.factor(table[[ grouping_variable ]]) ) {

        ## get factor levels and assign colors
        colors_for_groups <- setNames(
          default_colorset[seq_along(levels(table[[ grouping_variable ]]))],
          levels(table[[ grouping_variable ]])
        )

      ## ... characters
      } else if ( is.character(table[[ grouping_variable ]]) ) {

        ## get unique values and assign colors
        colors_for_groups <- setNames(
          default_colorset[seq_along(unique(table[[ grouping_variable ]]))],
          unique(table[[ grouping_variable ]])
        )
      }

    ## ... none of the above (e.g. numeric values)
    } else {
      colors_for_groups <- NULL
    }

    ##
    return(colors_for_groups)
  }

  ##--------------------------------------------------------------------------##
  ## Build hover info for projections.
  ##--------------------------------------------------------------------------##

  buildHoverInfoForProjections <- function(table) {

    ## put together cell ID, number of transcripts and number of expressed genes
    hover_info <- paste0(
      "<b>Cell</b>: ", table[[ "cell_barcode" ]], "<br>",
      "<b>Transcripts</b>: ", formatC(table[[ "nUMI" ]], format = "f", big.mark = ",", digits = 0), "<br>",
      "<b>Expressed genes</b>: ", formatC(table[[ "nGene" ]], format = "f", big.mark = ",", digits = 0), "<br>"
    )

    ## add info for known grouping variables
    for ( group in getGroups() ) {
      hover_info <- paste0(
        hover_info,
        "<b>", group, "</b>: ", table[[ group ]], "<br>"
      )
    }

    ##
    return(hover_info)
  }

  ##--------------------------------------------------------------------------##
  ## Randomly subset cells in data frame, if necessary.
  ##--------------------------------------------------------------------------##

  randomlySubsetCells <- function(table, percentage) {

    ## check if subsetting is necessary
    ## ... percentage is less than 100
    if ( percentage < 100 ) {

      ## calculate how many cells should be left after subsetting
      size_of_subset <- ceiling(percentage / 100 * nrow(table))

      ## get IDs of all cells
      cell_ids <- rownames(table)

      ## subset cell IDs
      subset_of_cell_ids <- cell_ids[ sample(seq_along(cell_ids), size_of_subset) ]

      ## subset table and return
      return(table[subset_of_cell_ids,])

    ## ... percentage is 100 -> no subsetting needed
    } else {

      ## return original table
      return(table)
    }
  }

  ##--------------------------------------------------------------------------##
  ## Tabs.
  ##--------------------------------------------------------------------------##

  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/load_data/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/overview/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/groups/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/most_expressed_genes/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/marker_genes/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/enriched_pathways/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/gene_expression/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/gene_id_conversion/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/trajectory/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/analysis_info/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/color_management/server.R"), local = TRUE)
  source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/about/server.R"), local = TRUE)

}
