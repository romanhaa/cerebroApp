##----------------------------------------------------------------------------##
## Tab: Color Management.
##----------------------------------------------------------------------------##

# UI element
output[["colors_for_samples_UI"]] <- renderUI({
  tagList(
    {
      color_list <- list()
      for ( i in 1:length(levels(sample_data()$cells$sample)) ) {
        sample_name <- names(sample_data()$samples$colors)[i]
        color_list[[i]] <- colourpicker::colourInput(
          paste0('color_sample_', sample_name),
          sample_name,
          sample_data()$samples$colors[i]
        )
      }
      color_list
    }
  )
})

# UI element
output[["colors_for_clusters_UI"]] <- renderUI({
  tagList(
    {
      color_list <- list()
      for ( i in 1:length(levels(sample_data()$cells$cluster)) ) {
        cluster_name <- names(sample_data()$clusters$colors)[i]
        color_list[[i]] <- colourpicker::colourInput(
          paste0('color_cluster_', cluster_name),
          paste0('Cluster: ', cluster_name),
          sample_data()$clusters$colors[i]
        )
      }
      color_list
    }
  )
})
