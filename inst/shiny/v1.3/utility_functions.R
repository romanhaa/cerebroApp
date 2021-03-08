##----------------------------------------------------------------------------##
## Functions to find columns of specific type (for automatic formatting).
##----------------------------------------------------------------------------##
findColumnsInteger <- function(df, columns_to_test) {
  columns_indices <- c()
  for ( i in columns_to_test ) {
    if (
      any(is.na(df[[i]])) == FALSE &&
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
      any(is.na(df[[i]])) == FALSE &&
      is.numeric(df[[i]]) &&
      min(df[[i]], na.rm = TRUE) >= 0 &&
      max(df[[i]], na.rm = TRUE) <= 100
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
      any(is.na(df[[i]])) == FALSE &&
      is.numeric(df[[i]]) &&
      min(df[[i]], na.rm = TRUE) >= 0 &&
      max(df[[i]], na.rm = TRUE) <= 1
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
      any(is.na(df[[i]])) == FALSE &&
      is.numeric(df[[i]])
    ) {
      columns_indices <- c(columns_indices, i)
    }
  }
  return(columns_indices)
}

##----------------------------------------------------------------------------##
## Functions to prepare and format table.
##----------------------------------------------------------------------------##
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
  if (
    hide_long_columns == TRUE &&
    length(columns_character) >= 1
  ) {
    for ( i in columns_character ) {
      if ( max(stringr::str_length(table[[i]]), na.rm = TRUE) > 200 ) {
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

  ## check whether percentage values were given on a 0-100 scale and convert
  ## them to 0-1 if so
  if (number_formatting == TRUE && length(columns_percent) > 0) {
    for (col in columns_percent) {
      col_name <- colnames(table)[col]
      if (max(table[,col_name] > 1)) {
        table[,col] <- table[,col] / 100
      }
    }
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

  # show cellular barcodes in monospace font
  if ('cell_barcode' %in% colnames(table_original)) {
    table <- table %>%
      DT::formatStyle(
        columns = which(colnames(table_original)=='cell_barcode'),
        target="cell", fontFamily="courier"
      )
  }

  ## if automatic number formatting is on...
  ## - remove decimals from integers
  ## - show 3 significant decimals for p-values
  ## - show 3 decimals for logFC
  ## - show percentage values with percent symbol and 2 decimals
  ## - show all other numeric values that are none of the above with 3
  ##   significant decimals
  if ( number_formatting == TRUE ) {

    ## integer values
    if (
      !is.null(columns_integer) &&
      length(columns_integer) > 0
    ) {
      table <- table %>%
        DT::formatRound(
          columns = columns_integer,
          digits = 0,
          interval = 3,
          mark = ","
        )
    }

    ## p-values
    if (
      !is.null(columns_p_value) &&
      length(columns_p_value) > 0
    ) {
      table <- table %>%
        DT::formatSignif(
          columns = columns_p_value,
          digits = 3
        )
    }

    ## logFC
    if (
      !is.null(columns_logFC) &&
      length(columns_logFC) > 0
    ) {
      table <- table %>%
        DT::formatRound(
          columns = columns_logFC,
          digits = 3
        )
    }

    ## percentage
    if (
      !is.null(columns_percent) &&
      length(columns_percent) > 0
    ) {
      table <- table %>%
      DT::formatPercentage(
        columns = columns_percent,
        digits = 2
      )
    }

    ## numeric but none of the above
    if (
      !is.null(columns_only_numeric) &&
      length(columns_only_numeric) > 0
    ) {
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
    if (
      !is.null(columns_logical) &&
      length(columns_logical) > 0
    ) {
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

##----------------------------------------------------------------------------##
## Function to prepare empty table.
##----------------------------------------------------------------------------##
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

##----------------------------------------------------------------------------##
## Function to calculate A-by-B tables, e.g. samples by clusters.
##----------------------------------------------------------------------------##
calculateTableAB <- function(
  table,
  groupA,
  groupB,
  mode,
  percent
) {

  ## TODO: more safety checks?

  ## check if specified group columns exist in table
  if ( groupA %in% colnames(table) == FALSE ) {
    stop(
      glue::glue(
        "Column specified as groupA (`{groupA}`) could not be found in meta ",
        "data."
      ),
      call. = FALSE
    )
  }

  if ( groupB %in% colnames(table) == FALSE ) {
    stop(
      glue::glue(
        "Column specified as groupB (`{groupB}`) could not be found in meta ",
        "data."
      ),
      call. = FALSE
    )
  }

  ## subset columns
  table <- table[,c(groupA, groupB)]

  ## factorize group columns A if not already a factor
  if ( is.character(table[[groupA]]) ) {
    levels_groupA <- table[[groupA]] %>% unique() %>% sort()
    table[,groupA] <- factor(table[[groupA]], levels = levels_groupA, exclude = NULL)
  } else {
    levels_groupA <- levels(table[,groupA])
  }

  ## factorize group columns B if not already a factor
  if ( is.character(table[[groupB]]) ) {
    levels_groupB <- table[[groupB]] %>% unique() %>% sort()
    table[,groupB] <- factor(table[[groupB]], levels = levels_groupB, exclude = NULL)
  } else {
    levels_groupB <- levels(table[,groupB])
  }

  ## prepare table in long format
  table <- table %>%
    dplyr::arrange(dplyr::across(c(groupA, groupB))) %>%
    dplyr::group_by(dplyr::across(c(groupA, groupB))) %>%
    dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
    dplyr::group_by(dplyr::across(c(groupA))) %>%
    dplyr::mutate(total_cell_count = sum(count)) %>%
    dplyr::ungroup()

  ## convert counts to percent
  if ( percent == TRUE ) {
    table <- table %>%
      dplyr::mutate(count = count / total_cell_count) %>%
      dplyr::select(
        tidyselect::all_of(c(groupA, "total_cell_count", groupB, "count"))
      )
  }

  ## bring table into wide format
  if ( mode == "wide" ) {

    table <- table %>%
      tidyr::pivot_wider(
        id_cols = tidyselect::all_of(c(groupA, "total_cell_count")),
        names_from = tidyselect::all_of(groupB),
        values_from = "count",
        values_fill = 0
      ) %>%
      dplyr::select(
        tidyselect::all_of(groupA), 'total_cell_count',
        tidyselect::any_of(levels_groupB)
      )

    ## fix order of columns if cell cycle info was chosen as second group
    if (
      'G1' %in% colnames(table) &&
      'G2M' %in% colnames(table) &&
      'S' %in% colnames(table)
    ) {
      table <- table %>%
        dplyr::select(
          tidyselect::all_of(c(groupA, 'total_cell_count', 'G1', 'S', 'G2M')),
          dplyr::everything()
        )
    }
  }

  ##
  return(table)
}

##----------------------------------------------------------------------------##
## Assign colors to groups.
##
## Provide table and column name, and this function will check whether the
## content of the column is categorical. If so, it will check whether colors
## have already been assigned to the levels/unique values and return those
## values. Otherwise, it will assign new colors from the default color set.
## The return value is a named vector.
##----------------------------------------------------------------------------##
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

##----------------------------------------------------------------------------##
## Build hover info for projections.
##----------------------------------------------------------------------------##
buildHoverInfoForProjections <- function(table) {
  ## put together cell ID, number of transcripts and number of expressed genes
  hover_info <- glue::glue(
    "<b>Cell</b>: {table[[ 'cell_barcode' ]]}<br>",
    "<b>Transcripts</b>: {formatC(table[[ 'nUMI' ]], format = 'f', big.mark = ',', digits = 0)}<br>",
    "<b>Expressed genes</b>: {formatC(table[[ 'nGene' ]], format = 'f', big.mark = ',', digits = 0)}"
  )
  ## add info for known grouping variables
  for ( group in getGroups() ) {
    hover_info <- glue::glue(
      "{hover_info}<br>",
      "<b>{group}</b>: {table[[ group ]]}"
    )
  }
  return(hover_info)
}

##----------------------------------------------------------------------------##
## Randomly subset cells in data frame, if necessary.
##----------------------------------------------------------------------------##
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

##----------------------------------------------------------------------------##
## Calculate X-Y ranges for projections.
##----------------------------------------------------------------------------##
getXYranges <- function(table) {
  ranges <- list(
    x = list(
      min = table[,1] %>% min(na.rm=TRUE) %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round(),
      max = table[,1] %>% max(na.rm=TRUE) %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
    ),
    y = list(
      min = table[,2] %>% min(na.rm=TRUE) %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round(),
      max = table[,2] %>% max(na.rm=TRUE) %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
    )
  )
  return(ranges)
}

##----------------------------------------------------------------------------##
## Function to get genes for selected gene set.
##----------------------------------------------------------------------------##
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
  msigdbr:::msigdbr_genesets[,1:2] %>%
  dplyr::filter(.data$gs_name == gene_set) %>%
  dplyr::inner_join(
    .,
    msigdbr:::msigdbr_genes,
    by = "gs_id"
  ) %>%
  dplyr::inner_join(
    .,
    msigdbr:::msigdbr_orthologs %>%
      dplyr::filter(.data$species_name == species) %>%
      dplyr::select(human_entrez_gene, gene_symbol),
    by = "human_entrez_gene"
  ) %>%
  dplyr::pull(gene_symbol) %>%
  unique() %>%
  sort()
}

##----------------------------------------------------------------------------##
## Function to calculate center of groups in projections/trajectories.
##----------------------------------------------------------------------------##
centerOfGroups <- function(coordinates, df, n_dimensions, group) {
  ## check number of dimenions in projection
  ## ... 2 dimensions
  if ( n_dimensions == 2 ) {
    ## calculate center for groups and return
    tidyr::tibble(
      x = coordinates[[1]],
      y = coordinates[[2]],
      group = df[[ group ]]
    ) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      x_median = median(x),
      y_median = median(y),
      .groups = 'drop_last'
    ) %>%
    dplyr::ungroup() %>%
    return()
  ## ... 3 dimensions
  } else if ( n_dimensions == 3 && is.numeric(coordinates[,3]) ) {
    ## calculate center for groups and return
    tidyr::tibble(
      x = coordinates[[1]],
      y = coordinates[[2]],
      z = coordinates[[3]],
      group = df[[ group ]]
    ) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      x_median = median(x),
      y_median = median(y),
      z_median = median(z),
      .groups = 'drop_last'
    ) %>%
    dplyr::ungroup() %>%
    return()
  }
}

##----------------------------------------------------------------------------##
## Set order of rows in data frame.
##----------------------------------------------------------------------------##
setRowOrder <- function(df, order) {
  if ( order == 'Random' ) {
    return(df[ sample(1:nrow(df)) , ])
  } else if ( order == "Highest expression on top" ) {
    return(dplyr::arrange(df, level))
  } else {
    return(df)
  }
}

##----------------------------------------------------------------------------##
## Functions to interact with data set.
##
## Never directly interact with data set: data_set()
##----------------------------------------------------------------------------##
getExperiment <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getExperiment())
  }
}
getParameters <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getParameters())
  }
}
getTechnicalInfo <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getTechnicalInfo())
  }
}
getGeneLists <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getGeneLists())
  }
}
getMeanExpressionForGenes <- function(genes) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getMeanExpressionForGenes(genes))
  }
}
getMeanExpressionForCells <- function(cells, genes) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getMeanExpressionForCells(cells, genes))
  }
}
getExpressionMatrix <- function(...) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getExpressionMatrix(...))
  }
}
getCellNames <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getCellNames())
  }
}
getGeneNames <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getGeneNames())
  }
}
getGroups <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getGroups())
  }
}
getGroupLevels <- function(group) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getGroupLevels(group))
  }
}
getCellCycle <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getCellCycle())
  }
}
getMetaData <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getMetaData())
  }
}
availableProjections <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$availableProjections())
  }
}
getProjection <- function(name) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getProjection(name))
  }
}
getTree <- function(group) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getTree(group))
  }
}
getGroupsWithMostExpressedGenes <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getGroupsWithMostExpressedGenes())
  }
}
getMostExpressedGenes <- function(group) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getMostExpressedGenes(group))
  }
}
getMethodsForMarkerGenes <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getMethodsForMarkerGenes())
  }
}
getGroupsWithMarkerGenes <- function(method) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getGroupsWithMarkerGenes(method))
  }
}
getMarkerGenes <- function(method, group) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getMarkerGenes(method, group))
  }
}
getMethodsForEnrichedPathways <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getMethodsForEnrichedPathways())
  }
}
getGroupsWithEnrichedPathways <- function(method) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getGroupsWithEnrichedPathways(method))
  }
}
getEnrichedPathways <- function(method, group) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getEnrichedPathways(method, group))
  }
}
getMethodsForTrajectories <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getMethodsForTrajectories())
  }
}
getNamesOfTrajectories <- function(method) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getNamesOfTrajectories(method))
  }
}
getTrajectory <- function(method, name) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getTrajectory(method, name))
  }
}
getExtraMaterialCategories <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getExtraMaterialCategories())
  }
}
checkForExtraTables <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$checkForExtraTables())
  }
}
getNamesOfExtraTables <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getNamesOfExtraTables())
  }
}
getExtraTable <- function(name) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getExtraTable(name))
  }
}
checkForExtraPlots <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$checkForExtraPlots())
  }
}
getNamesOfExtraPlots <- function() {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getNamesOfExtraPlots())
  }
}
getExtraPlot <- function(name) {
  if ( 'Cerebro_v1.3' %in% class(data_set()) ) {
    return(data_set()$getExtraPlot(name))
  }
}
