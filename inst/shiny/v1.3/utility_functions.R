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
