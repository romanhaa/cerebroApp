##----------------------------------------------------------------------------##
## Color management.
##----------------------------------------------------------------------------##
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

##----------------------------------------------------------------------------##
## Assign colors to groups.
##----------------------------------------------------------------------------##
reactive_colors <- reactive({
  req(data_set())
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
      if ( 'N/A' %in% getGroupLevels(group_name) ) {
        colors[[ group_name ]][ which(names(colors[[ group_name ]]) == 'N/A') ] <- '#898989'
      }
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
