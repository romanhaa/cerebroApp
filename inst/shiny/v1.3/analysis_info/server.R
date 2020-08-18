##----------------------------------------------------------------------------##
## Tab: Analysis info
##----------------------------------------------------------------------------##

# general info
output[["data_set_info_general"]] <- renderText({

  ## collect general info
  general_info <- paste0(
    "<strong><u>General</u></strong>",
    "<ul>",
      "<li><b>Date of analysis:</b> ",
      getExperiment()$date_of_analysis,
      "<li><b>Date of export:</b> ",
      getExperiment()$date_of_export,
      "<li><b>Experiment name:</b> ",
      getExperiment()$experiment_name,
      "<li><b>Organism:</b> ",
      getExperiment()$organism,
    "</ul>"
  )

  ## collect group info
  groups_raw <- getGroups()
  groups_content <- c()
  for ( i in groups_raw ) {
    group_levels <- getGroupLevels(i)
    groups_content <- c(
      groups_content,
      paste0(
        "<li><b>", i, " (", length(group_levels) ,"):</b> ",
        paste0(group_levels, collapse = ", ")
      )
    )
  }
  groups_text <- paste0(
    "<strong><u>Grouping variables</u></strong>",
    "<ul>",
    paste0(groups_content, collapse = ""),
    "</ul>"
  )

  ## collect parameters
  parameters_raw <- getParameters()
  parameters_content <- c()
  ## go through list of parameters
  for ( i in 1:length(parameters_raw)) {
    ## ... if item is a list itself
    if ( is.list(parameters_raw[[i]]) ) {
      ## prepare sub-header and initiate new list
      temp_parameters_content <- c("<li><b>", names(parameters_raw)[i],":</b><ul>")
      ## add items to new list
      for ( j in 1:length(parameters_raw[[i]])) {
        ## prepare HTML version of info
        temp_parameters_content_sub <- paste0(
          "<li><b>", names(parameters_raw[[i]])[j], ":</b> ", parameters_raw[[i]][[j]]
        )
        ## add new HTML piece to existing string
        temp_parameters_content <- c(temp_parameters_content, temp_parameters_content_sub)
      }
      ## close list
      temp_parameters_content <- c(temp_parameters_content, "</ul>")
    ## ... if item is not a list
    } else {
      ## prepare HTML version of info
      temp_parameters_content <- paste0(
        "<li><b>", names(parameters_raw)[i], ":</b> ", parameters_raw[[i]]
      )
    }
    ## add new HTML piece to existing string
    parameters_content <- c(parameters_content, temp_parameters_content)
  }
  ## put all HTML texts together
  parameters_text <- paste0(
    "<strong><u>Parameters</u></strong>",
    "<ul>",
      paste0(parameters_content, collapse = ""),
    "</ul>"
  )

  ## collect gene lists
  gene_lists_raw <- getGeneLists()
  gene_lists_content <- c()
  for ( i in 1:length(gene_lists_raw)) {
    temp_gene_lists_content <- paste0(
      "<li><b>", names(gene_lists_raw)[i], ":</b> ",
      paste0(gene_lists_raw[[i]], collapse = ", ")
    )
    gene_lists_content <- c(gene_lists_content, temp_gene_lists_content)
  }
  gene_lists_text <- paste0(
    "<strong><u>Gene lists</u></strong>",
    "<ul>",
    paste0(gene_lists_content, collapse = ""),
    "</ul>"
  )

  # "<strong><u>Marker genes</u></strong>",
  # "<ul>",
  #   "<li><b>Only positive:</b> ",
  #   sample_data()$marker_genes$parameters$only_positive,
  #   "<li><b>Fraction of cells in group of interest that must express marker gene:</b> ",
  #   sample_data()$marker_genes$parameters$minimum_percentage,
  #   "<li><b>LogFC threshold:</b> ",
  #   sample_data()$marker_genes$parameters$logFC_threshold,
  #   "<li><b>p-value threshold:</b> ",
  #   sample_data()$marker_genes$parameters$p_value_threshold,
  #   "<li><b>Test:</b> ",
  #   sample_data()$marker_genes$parameters$test,
  # "</ul>",
  # "<strong><u>Pathway enrichment</u></strong>",
  # "<ul>",
  #   "<li><b>Enrichr:</b>",
  #   "<ul>",
  #     "<li><b>Databases:</b> ",
  #     paste0(sample_data()$enriched_pathways$enrichr$parameters$databases, collapse = ", "),
  #     "<li><b>Adj. p-value cut-off:</b> ",
  #     sample_data()$enriched_pathways$enrichr$parameters$adj_p_cutoff,
  #     "<li><b>Max. terms:</b> ",
  #     sample_data()$enriched_pathways$enrichr$parameters$max_terms,
  #   "</ul>",
  #   "<li><b>GSVA:</b>",
  #   "<ul>",
  #     "<li><b>GMT file:</b> ",
  #     sample_data()$enriched_pathways$GSVA$parameters$GMT_file,
  #     "<li><b>p-value cut-off:</b> ",
  #     sample_data()$enriched_pathways$GSVA$parameters$thresh_p_val,
  #     "<li><b>q-value cut-off:</b> ",
  #     sample_data()$enriched_pathways$GSVA$parameters$thresh_q_val,
  #   "</ul>",
  # "</ul>"

  ## collect technical info
  technical_info_raw <- getTechnicalInfo()
  info_R_raw <- technical_info_raw$R
  info_R <- c()
  for ( i in 1:length(info_R_raw) ) {
    info_R <- paste(info_R, "<br>", info_R_raw[i])
  }
  technical_info_raw$R <- NULL
  technical_info_content <- c()
  for ( i in 1:length(technical_info_raw)) {
    temp_technical_info_content <- paste0(
      "<li><b>", names(technical_info_raw)[i], ":</b> ", technical_info_raw[[i]]
    )
    technical_info_content <- c(technical_info_content, temp_technical_info_content)
  }
  technical_info_text <- paste0(
    "<strong><u>Technical info</u></strong>",
    "<ul>",
      paste0(technical_info_content, collapse = ""),
      "<li><strong>Session info:</strong> ",
    "</ul>",
    "<pre>",
    info_R,
    "</pre>"
  )


  # technical_info <- paste0(
  #   "<strong><u>Technical info</u></strong>",
  #   "<ul>",
  #     "<li><strong>cerebroApp version:</strong> ",
  #     getTechnicalInfo()$cerebroApp_version,
  #     "<li><strong>Seurat version:</strong> ",
  #     getTechnicalInfo()$seurat_version,
  #     "<li><strong>R session info:</strong> ",
  #   "</ul>",
  #   "<pre>",
  #   info_R,
  #   "</pre>"
  # )

  ## merge all parts
  paste0(
    general_info,
    groups_text,
    parameters_text,
    gene_lists_text,
    technical_info_text
  )
})
