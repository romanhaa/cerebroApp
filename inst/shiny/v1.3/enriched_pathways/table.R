##----------------------------------------------------------------------------##
## Tab: Enriched pathways
##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["enriched_pathways_table_UI"]] <- renderUI({
  selected_method <- input[["enriched_pathways_selected_method"]]
  selected_table <- input[["enriched_pathways_selected_table"]]
  if (
    is.null(selected_method) ||
    selected_method %in% getMethodsForEnrichedPathways() == FALSE
  ) {
    fluidRow(
      cerebroBox(
        title = boxTitle("Enriched pathways"),
        textOutput("enriched_pathways_message_no_method_found")
      )
    )
  } else {
    req(
      input[["enriched_pathways_selected_method"]],
      input[["enriched_pathways_selected_table"]]
    )
    fluidRow(
      cerebroBox(
        title = tagList(
          boxTitle("Enriched pathways"),
          cerebroInfoButton("enriched_pathways_info")
        ),
        uiOutput("enriched_pathways_table_or_text_UI")
      )
    )
  }
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
    input[["enriched_pathways_selected_table"]],
    input[["enriched_pathways_selected_table"]] %in% getGroupsWithEnrichedPathways(input[["enriched_pathways_selected_method"]])
  )
  results_type <- getEnrichedPathways(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_table"]]
  )
  ## depending on the content of the results slot, show a text message or
  ## switches and table
  if (
    is.character(results_type) &&
    results_type == "no_markers_found"
  ) {
    textOutput("enriched_pathways_message_no_markers_found")
  } else if (
    is.character(results_type) &&
    results_type == "no_pathways_found"
  ) {
    textOutput("enriched_pathways_message_no_pathways_found")
  } else if (
    is.character(results_type) &&
    results_type == "no_gene_sets_enriched"
  ) {
    textOutput("enriched_pathways_message_no_gene_sets_enriched")
  } else if ( is.data.frame(results_type) ) {
    fluidRow(
      column(12,
        shinyWidgets::materialSwitch(
          inputId = "enriched_pathways_table_filter_switch",
          label = "Show results for all subgroups (no pre-filtering):",
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
          value = TRUE,
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

  ##
  req(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_table"]],
    input[["enriched_pathways_selected_table"]] %in% getGroupsWithEnrichedPathways(input[["enriched_pathways_selected_method"]]),
    !is.null(input[["enriched_pathways_table_filter_switch"]])
  )

  ## fetch results
  results_df <- getEnrichedPathways(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_table"]]
  )

  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))

  ## check if pre-filtering is activated and name of first column in table is
  ## one of the registered groups
  ## ... it's not
  if (
    input[["enriched_pathways_table_filter_switch"]] == TRUE ||
    colnames(results_df)[1] %in% getGroups() == FALSE
  ) {

    ## return nothing (empty row)
    fluidRow()

  ## ... it is
  } else {

    ## check for which groups results exist
    if ( is.character(results_df[[1]]) ) {
      available_groups <- unique(results_df[[1]])
    } else if ( is.factor(results_df[[1]]) ) {
      available_groups <- levels(results_df[[1]])
    }

    ## create input selection for available groups
    fluidRow(
      column(12,
        selectInput(
          "enriched_pathways_table_select_group_level",
          label = "Filter results for subgroup:",
          choices = available_groups
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
    input[["enriched_pathways_selected_table"]],
    input[["enriched_pathways_selected_table"]] %in% getGroupsWithEnrichedPathways(input[["enriched_pathways_selected_method"]])
  )

  ## fetch results
  results_df <- getEnrichedPathways(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_table"]]
  )

  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))

  ## filter the table for a specific subgroup only if specified by the user
  ## (otherwise show all results)
  if (
    input[["enriched_pathways_table_filter_switch"]] == FALSE &&
    colnames(results_df)[1] %in% getGroups() == TRUE
  ) {

    ## don't proceed if selection of subgroup is not available
    req(
      input[["enriched_pathways_table_select_group_level"]]
    )

    ## filter table
    results_df <- results_df[ which(results_df[[1]] == input[["enriched_pathways_table_select_group_level"]]) , ]
  }

  ## if the table is empty, e.g. because the filtering of results for a specific
  ## subgroup did not work properly, skip the processing and show and empty
  ## table (otherwise the procedure would result in an error)
  if ( nrow(results_df) == 0 ) {
    results_df %>%
    as.data.frame() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()

  ## ... if there is at least 1 row, create proper table
  } else if ( nrow(results_df) > 0 ) {

    ## check if data frame comes from the enrichR cerebroApp function
    columns_hide <- c()
    if (
      any(grepl(colnames(results_df), pattern = "Term")) &&
      any(grepl(colnames(results_df), pattern = "Old.P.value")) &&
      any(grepl(colnames(results_df), pattern = "Old.Adjusted.P.value"))
    ) {
      columns_hide <- c()
      columns_hide <- c(columns_hide, grep(colnames(results_df), pattern = "Old.P.value"))
      columns_hide <- c(columns_hide, grep(colnames(results_df), pattern = "Old.Adjusted.P.value"))
    }

    prettifyTable(
      results_df,
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
        input[["enriched_pathways_selected_table"]]
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
  "No marker genes were identified for any of the subpopulations of this grouping variable, which are required to perform pathway enrichment analysis with Enrichr."
})

##----------------------------------------------------------------------------##
## Alternative text message if no pathways were enriched (Enrichr).
##----------------------------------------------------------------------------##

output[["enriched_pathways_message_no_pathways_found"]] <- renderText({
  "Enrichr did not find any pathway to be enriched in any subpopulation of this grouping variable."
})

##----------------------------------------------------------------------------##
## Alternative text message if no gene sets were enriched (GSVA).
##----------------------------------------------------------------------------##

output[["enriched_pathways_message_no_gene_sets_enriched"]] <- renderText({
  "No gene sets were found to be enriched (considering the selected statistical thresholds) by GSVA in any of the subpopulations of this grouping variable."
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
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

enriched_pathways_info <- list(
  title = "Enriched pathways",
  text = HTML("
    At the moment, Cerebro supports two different ways to perform pathway enrichment analysis (Enrichr, GSVA). However, in principle results from any method or tool can be added to the Cerebro object.<br>
    <br>
    <b>Enrichr</b><br>
    Using all marker genes identified for a respective group of cells, gene list enrichment analysis is performed using the Enrichr API, including gene ontology terms, KEGG and Wiki Pathways, BioCarta and many others. Terms are sorted based on the combined score. By default, the genes that overlap between the marker gene list and a term are not shown (for better visibility) but the column can be added using the 'Column visibility' button. For the details on the combined score is calculated, please refer to the <a target='_blank' href='http://amp.pharm.mssm.edu/Enrichr/'>Enrichr website</a> and publication.<br>
    <br>
    <b>GSVA</b><br>
    GSVA (Gene Set Variation Analysis) is a method to perform gene set enrichment analysis. Diaz-Mejia and colleagues found GSVA to perform well compared to other tools ('Evaluation of methods to assign cell type labels to cell clusters from single-cell RNA-sequencing data' F1000Research, 2019). Statistics (p-value and adj. p-value) are calculated as done by Diaz-Mejia et al. Columns with the gene lists for each term and the enrichment score are hidden by default but can be made visible through the 'Column visibility' button. More details about GSVA can be found on the <a target='_blank', href='https://bioconductor.org/packages/release/bioc/html/GSVA.html'>GSVA Bioconductor page</a>.
    <h4>Options</h4>
    <b>Show results for all subgroups (no pre-filtering)</b><br>
    When active, the subgroup section element will disappear and instead the table will be shown for all subgroups. Subgroups can still be selected through the dedicated column filter, which also allows to select multiple subgroups at once. While using the column filter is more elegant, it can become laggy with very large tables, hence to option to filter the table beforehand. Please note that this feature only works if the first column was recognized as holding assignments to one of the grouping variables, e.g. 'sample' or 'clusters', otherwise your choice here will be ignored and the whole table shown without pre-filtering.<br>
    <b>Automatically format numbers</b><br>
    When active, columns in the table that contain different types of numeric values will be formatted based on what they <u>seem</u> to be. The algorithm will look for integers (no decimal values), percentages, p-values, log-fold changes and apply different formatting schemes to each of them. Importantly, this process does that always work perfectly. If it fails and hinders working with the table, automatic formatting can be deactivated.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <b>Highlight values with colors</b><br>
    Similar to the automatic formatting option, when active, Cerebro will look for known columns in the table (those that contain grouping variables), try to interpret column content, and use colors and other stylistic elements to facilitate quick interpretation of the values. If you prefer the table without colors and/or the identification does not work properly, you can simply deactivate this feature.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
