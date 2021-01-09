##----------------------------------------------------------------------------##
## UI elements to set group filters for the projection.
##----------------------------------------------------------------------------##
output[["overview_projection_group_filters_UI"]] <- renderUI({
  group_filters <- list()
  for ( i in getGroups() ) {
    group_filters[[i]] <- shinyWidgets::pickerInput(
      paste0("overview_projection_group_filter_", i),
      label = i,
      choices = getGroupLevels(i),
      selected = getGroupLevels(i),
      options = list(
        "actions-box" = TRUE
      ),
      multiple = TRUE
    )
  }
  return(group_filters)
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_group_filters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_projection_group_filters_info"]], {
  showModal(
    modalDialog(
      overview_projection_group_filters_info[["text"]],
      title = overview_projection_group_filters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
overview_projection_group_filters_info <- list(
  title = "Group filters for projection",
  text = HTML("
    The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that are pass all filters (for each grouping variable) are shown in the projection.
    "
  )
)

##----------------------------------------------------------------------------##
## example for implementation of nested checkboxes with shinyTree for selection
## of group levels to show; works similar to cellxgene; anyway decided against
## it because it creates a new dependency and isn't as aesthetically pleasing as
## the existing solution
##----------------------------------------------------------------------------##

# output[["overview_projection_group_filters_tree"]] <- shinyTree::renderTree({
#   groups <- list()
#   for ( i in getGroups() ) {
#     groups[[i]] <- structure(
#       as.list(
#         setNames(
#           getGroupLevels(i),
#           getGroupLevels(i)
#         )
#       ),
#       stselected = TRUE
#     )
#   }
#   groups
# })

# output[["overview_projection_group_filters_selected_groups"]] <- renderPrint({
#   tree <- input[["overview_projection_group_filters_tree"]]
#   req(overview_projection_group_filters_tree)
#   str(shinyTree::get_selected(tree, format = "slices"))
# })

# output[["overview_projection_group_filters_tree_UI"]] <- renderUI({
#   tagList(
#     shinyTree::shinyTree(
#       "overview_projection_group_filters_tree",
#       theme = "proton",
#       themeIcons = FALSE,
#       themeDots = FALSE,
#       checkbox = TRUE
#     ),
#     verbatimTextOutput("sel_slices")
#   )
# })
