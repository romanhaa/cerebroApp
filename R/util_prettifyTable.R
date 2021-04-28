#' @title test
#' @description Function to prepare and format table.
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test
.prettifyTable <- function(
  table,
  groups_in_dataset,
  cell_cycle_columns,
  group_colors,
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
  columns_integer <- .findColumnsInteger(table, columns_numeric)
  ## identify which columns might contain percentages, p-values, and logFC
  columns_percent <- .findColumnsPercentage(table)
  columns_p_value <- .findColumnsPValues(table)
  columns_logFC <- .findColumnsLogFC(table)
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
    columns_groups <- which(colnames(table_original) %in% groups_in_dataset)
    if ( length(columns_groups) > 0 ) {
      for ( i in columns_groups ) {
        group <- colnames(table_original)[i]
        if ( all(unique(table_original[[i]]) %in% names(group_colors[[group]])) ) {
          table <- table %>%
            DT::formatStyle(
              i,
              backgroundColor = DT::styleEqual(
                names(group_colors[[group]]),
                group_colors[[group]]
              ),
              fontWeight = 'bold'
            )
        }
      }
    }
    ## cell cycle assignments
    columns_cell_cycle <- which(colnames(table_original) %in% cell_cycle_columns)
    if ( length(columns_cell_cycle) > 0 ) {
      for ( i in columns_cell_cycle ) {
        method <- colnames(table_original)[i]
        if ( all(unique(table_original[[i]]) %in% names(group_colors[[method]])) ) {
          table <- table %>%
            DT::formatStyle(
              i,
              backgroundColor = DT::styleEqual(
                names(group_colors[[method]]),
                group_colors[[method]]
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
