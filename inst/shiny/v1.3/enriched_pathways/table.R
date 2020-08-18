##----------------------------------------------------------------------------##
## Tab: Enriched pathways
##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["enriched_pathways_table_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Enriched pathways"),
        cerebroInfoButton("enriched_pathways_info")
      ),
      uiOutput("enriched_pathways_table_or_text_UI")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that shows table and toggle switches (for sub-filtering of
## results, automatic number formatting, automatic coloring of values), or text
## messages if no marker genes or enriched pathways were found or data is
## missing.
##----------------------------------------------------------------------------##

output[["enriched_pathways_table_or_text_UI"]] <- renderUI({
  req(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_group"]]
  )

  ## fetch results
  results <- getEnrichedPathways(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_group"]]
  )

  ## TODO: probably some of these outputs are not needed anymore, check with
  ##       function that generates the output
  if (
    is.character(results) &&
    results == "no_markers_found"
  ) {
    textOutput("enriched_pathways_message_no_markers_found")
  } else if (
    is.character(results) &&
    results == "no_gene_sets_enriched"
  ) {
    textOutput("enriched_pathways_message_no_gene_sets_enriched")
  } else if (
    is.character(results) &&
    ## TODO: adjust output of function to catch this message
    results == "only_one_group_level"
  ) {
    textOutput("enriched_pathways_message_only_one_group_level")
  } else if ( is.data.frame(results) ) {
    fluidRow(
      column(12,
        shinyWidgets::materialSwitch(
          inputId = "enriched_pathways_table_filter_switch",
          label = "Show results for all subgroups (no filtering):",
          value = FALSE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "enriched_pathways_table_number_formatting",
          label = "Automatically format numbers:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "enriched_pathways_table_color_highlighting",
          label = "Highlight values with colors:",
          value = FALSE,
          status = "primary",
          inline = TRUE
        )
      ),
      column(12,
        uiOutput("enriched_pathways_filter_subgroups_UI")
      ),
      column(12,
        DT::dataTableOutput("enriched_pathways_table")
      )
    )
  } else {
    textOutput("enriched_pathways_message_no_data_found")
  }
})

##----------------------------------------------------------------------------##
## UI element for sub-filtering of results if toggled.
##----------------------------------------------------------------------------##

output[["enriched_pathways_filter_subgroups_UI"]] <- renderUI({
  req(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_group"]]
  )
  if ( input[["enriched_pathways_table_filter_switch"]] == TRUE ) {
    fluidRow()
  } else {
    fluidRow(
      column(12,
        selectInput(
          "enriched_pathways_table_select_group_level",
          label = "Filter results for subgroup:",
          choices = getGroupLevels(input[["enriched_pathways_selected_group"]])
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##

output[["enriched_pathways_table"]] <- DT::renderDataTable(server = FALSE, {

  ##
  req(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_group"]],
    input[["enriched_pathways_table_select_group_level"]]
  )

  ## fetch results
  table <- getEnrichedPathways(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_group"]]
  )

  req(
    is.data.frame(table)
  )

  ## filter the table for a specific subgroup only if specified by the user
  ## (otherwise show all results)
  if ( input[["enriched_pathways_table_filter_switch"]] == FALSE ) {
    table <- table %>%
      dplyr::filter_at(1, dplyr::all_vars(. == input[["enriched_pathways_table_select_group_level"]]))
  }

  ## if the table is empty, e.g. because the filtering of results for a specific
  ## subgroup did not work properly, skip the processing and show and empty
  ## table (otherwise the procedure would result in an error)
  if ( nrow(table) == 0 ) {
    table %>%
    as.data.frame() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()
  ## if there is at least 1 row, create proper table
  } else if ( nrow(table) > 0 ) {

    ## check if data frame comes from the enrichR cerebroApp function
    columns_hide <- c()
    if (
      any(grepl(colnames(table), pattern = "Term")) &&
      any(grepl(colnames(table), pattern = "Old.P.value")) &&
      any(grepl(colnames(table), pattern = "Old.Adjusted.P.value"))
    ) {
      columns_hide <- c()
      columns_hide <- c(columns_hide, grep(colnames(table), pattern = "Old.P.value"))
      columns_hide <- c(columns_hide, grep(colnames(table), pattern = "Old.Adjusted.P.value"))
    }

    prettifyTable(
      table,
      filter = list(position = "top", clear = TRUE),
      dom = "Bfrtlip",
      show_buttons = TRUE,
      number_formatting = input[["enriched_pathways_table_number_formatting"]],
      color_highlighting = input[["enriched_pathways_table_color_highlighting"]],
      hide_long_columns = TRUE,
      columns_hide = columns_hide,
      download_file_name = paste0(
        "enriched_pathways_by_",
        input[["enriched_pathways_selected_method"]], "_",
        input[["enriched_pathways_selected_group"]]
      ),
      page_length_default = 20,
      page_length_menu = c(20, 50, 100)
    )
  }

})

##----------------------------------------------------------------------------##
## Alternative text message if no marker genes were found.
##----------------------------------------------------------------------------##

output[["enriched_pathways_message_no_markers_found"]] <- renderText({
  "No marker genes were identified for this group, which are required to perform pathway enrichment analysis with Enrichr."
})

##----------------------------------------------------------------------------##
## Alternative text message if no pathways were enriched.
##----------------------------------------------------------------------------##

output[["enriched_pathways_message_no_gene_sets_enriched"]] <- renderText({
  "No gene sets were found to be enriched (with the selected statistical thresholds) in any group."
})

##----------------------------------------------------------------------------##
## Alternative text message if there is only one level in the group.
##----------------------------------------------------------------------------##

output[["enriched_pathways_message_only_one_group_level"]] <- renderText({
  "The selected grouping variable consists of a single level which means pathway enrichment analysis cannot be applied."
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##

output[["enriched_pathways_message_no_data_found"]] <- renderText({
  "Data not available or not in correct format (data frame)."
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["enriched_pathways_info"]], {
  showModal(
    modalDialog(
      enriched_pathways_info[["text"]],
      title = enriched_pathways_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

## TODO: update description
enriched_pathways_info <- list(
  title = "Enriched pathways",
  text = HTML(
    "<p>At the moment, Cerebro supports to perform pathway enrichment analysis through two methods which work in different ways: Enrichr, GSVA.<br>
    <br>
    <b>Enrichr</b><br>
    Using all marker genes identified for a respective sample, gene list enrichment analysis is performed using the Enrichr API, including gene ontology terms, KEGG and Wiki Pathways, BioCarta and many others. Terms are sorted based on the combined score. By default, the genes that overlap between the marker gene list and a term are not shown (for better visibility) but the column can be added using the 'Column visibility' button. For the details on the combined score is calculated, please refer to the <a target='_blank' href='http://amp.pharm.mssm.edu/Enrichr/'>Enrichr website</a> and publication.<br>
    <br>
    <b>GSVA</b><br>
    GSVA (Gene Set Variation Analysis) is a method to perform gene set enrichment analysis. Diaz-Mejia and colleagues found GSVA to perform well compared to other tools ('Evaluation of methods to assign cell type labels to cell clusters from single-cell RNA-sequencing data' F1000Research, 2019). Statistics (p-value and adj. p-value) are calculated as done by Diaz-Mejia et al. Columns with the gene lists for each term and the enrichment score are hidden by default but can be made visible through the 'Column visibility' button. More details about GSVA can be found on the <a target='_blank', href='https://bioconductor.org/packages/release/bioc/html/GSVA.html'>GSVA Bioconductor page</a>."
  )
)
