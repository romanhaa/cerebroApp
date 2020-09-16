##----------------------------------------------------------------------------##
## Tab: Color management
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with color selection boxes for each level in each grouping
## variable.
##----------------------------------------------------------------------------##

output[["color_assignments_UI"]] <- renderUI({
  fluidRow(
    tagList({
      group_list <- list()
      for ( group_name in getGroups() ) {
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
            for ( group_level in getGroupLevels(group_name) ) {
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
      if ( length(getCellCycle()) > 0 ) {
        for ( column in getCellCycle() ) {
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
              for ( state in unique(as.character(getMetaData()[[ column ]])) ) {
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
  )

})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["color_assignments_info"]], {
  showModal(
    modalDialog(
      color_assignments_info[["text"]],
      title = color_assignments_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

color_assignments_info <- list(
  title = "Colors for groups",
  text = p("Using this interface, you can assign new colors to each of the groups which will then be used across all tabs in Cerebro.")
)
