##----------------------------------------------------------------------------##
## Tab: Color Management.
##----------------------------------------------------------------------------##

# UI element
output[["color_assignments_UI"]] <- renderUI({



  tagList({
    group_list <- list()
    for ( group_name in sample_data()$getGroups() )
    {
      group_list[[ group_name ]] <- box(
        title = tagList(
          boxTitle(group_name),
          cerebroInfoButton("color_assignments_info")
        ),
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        collapsible = TRUE,
        tagList({
          color_list <- list()
          for ( group_level in sample_data()$getGroupLevels(group_name) )
          {
            color_list[[ group_level ]] <- colourpicker::colourInput(
              inputId = paste0('color_', group_name, '_', gsub(group_level, pattern = '[^[:alnum:]]', replacement = '_')),
              label = group_level,
              value = reactive_colors()[[ group_name ]][ group_level ]
            )
          }
          color_list
        })
      )
    }

    ## if there are columns with cell cycle info, add color selection elements
    ## also for those
    if ( length(sample_data()$cell_cycle) > 0 )
    {
      for ( column in sample_data()$cell_cycle )
      {
        group_list[[ column ]] <- box(
          title = tagList(
            boxTitle(column),
            cerebroInfoButton("color_assignments_info")
          ),
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          collapsible = TRUE,
          tagList({
            color_list <- list()
            for ( state in unique(as.character(colData(sample_data()$expression)[[ column ]])) )
            {
              color_list[[ state ]] <- colourpicker::colourInput(
                inputId = paste0('color_', column, '_', gsub(state, pattern = '[^[:alnum:]]', replacement = '_')),
                label = state,
                value = reactive_colors()[[ column ]][ state ]
              )
            }
            color_list
          })
        )
      }
    }

    group_list
  })

})

# info box
observeEvent(input[["color_assignments_info"]],
{
  showModal(
    modalDialog(
      color_assignments_info[["text"]],
      title = color_assignments_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})
