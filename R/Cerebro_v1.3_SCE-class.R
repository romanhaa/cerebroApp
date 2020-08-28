# setOldClass(Classes = 'package_version')

# #' @title
# #' R6 class in which data sets will be stored for visualization in Cerebro.
# #'
# #' @description
# #' A \code{Cerebro_v1.3_SCE} object is an R6 class that contains several types
# #' of data that can be visualized in Cerebro. In contrast to the
# #' \code{Cerebro_v1.3} object, it internally uses a \code{SingleCellExperiment}
# #' object to store transcript counts, cell meta data, and projections.
# #' 
# #' @return
# #' A new \code{Cerebro_v1.3_SCE} object.
# #'
# #' @importFrom R6 R6Class
# #'
# #' @export
# #'
# Cerebro_v1.3_SCE <- R6::R6Class(
#   'Cerebro_v1.3_SCE',

#   ##--------------------------------------------------------------------------##
#   ## fields/slots in the object
#   ##--------------------------------------------------------------------------##

#   public = list(

#     #' @field version cerebroApp version that was used to create the object.
#     version = c(),

#     #' @field experiment \code{list} that contains meta data about the data set,
#     #' including experiment name, species, date of export.
#     experiment = list(),

#     #' @field technical_info \code{list} that contains technical information
#     #' about the analysis, including the R session info.
#     technical_info = list(),

#     #' @field parameters \code{list} that contains important parameters that
#     #' were used during the analysis, e.g. cut-off values for cell filtering.
#     parameters = list(),

#     #' @field groups \code{list} that contains specified grouping variables and
#     #' and the group levels (subgroups) that belong to each of them. For each
#     #' grouping variable, a corresponding column with the same name must exist
#     #' in the meta data.
#     groups = list(),

#     #' @field cell_cycle \code{vector} that contains the name of columns in the
#     #' meta data that contain cell cycle assignments.
#     cell_cycle = c(),

#     #' @field gene_lists \code{list} that contains gene lists, e.g.
#     #' mitochondrial and/or ribosomal genes.
#     gene_lists = list(),

#     #' @field expression \code{\link{SingleCellExperiment}} object that holds
#     #' meta data, transcript counts, and dimensional reductions.
#     expression = SingleCellExperiment(),

#     #' @field most_expressed_genes \code{list} that contains a \code{data.frame}
#     #' holding the most expressed genes for each grouping variable that was
#     #' specified during the call to \code{\link{getMostExpressedGenes}}.
#     most_expressed_genes = list(),

#     #' @field marker_genes \code{list} that contains a \code{list} for every
#     #' method that was used to calculate marker genes, and a \code{data.frame}
#     #' for each grouping variable, e.g. those that were specified during the
#     #' call to \code{\link{getMarkerGenes}}.
#     marker_genes = list(),

#     #' @field enriched_pathways \code{list} that contains a \code{list} for
#     #' every method that was used to calculate marker genes, and a
#     #' \code{data.frame} for each grouping variable, e.g. those that were
#     #' specified during the call to \code{\link{getEnrichedPathways}} or
#     #' \code{\link{performGeneSetEnrichmentAnalysis}}.
#     enriched_pathways = list(),

#     #' @field trees \code{list} that contains a phylogenetic tree (class
#     #' \code{phylo}) for grouping variables.
#     trees = list(),

#     #' @field trajectories \code{list} that contains a \code{list} for every
#     #' method that was used to calculate trajectories, and, depending on the
#     #' method, a \code{data.frame} or \code{list} for each specific trajectory,
#     #' e.g. those extracted with \code{\link{extractMonocleTrajectory}}.
#     trajectories = list(),

#     ##------------------------------------------------------------------------##
#     ## methods to interact with the object
#     ##------------------------------------------------------------------------##

#     #' @description
#     #' Create a new \code{Cerebro_v1.3} object.
#     #'
#     #' @return
#     #' A new \code{Cerebro_v1.3} object.
#     initialize = function() {
#       self$experiment <- list(
#         experiment_name = NULL,
#         organism = NULL,
#         date_of_analysis = NULL,
#         date_of_export = NULL
#       )
#     },

#     #' @description
#     #' Set the version of \code{cerebroApp} that was used to generate this
#     #' object.
#     setVersion = function(x) {
#       self$version <- x
#     },

#     #' @description
#     #' Get the version of \code{cerebroApp} that was used to generate this
#     #' object.
#     #' 
#     #' @return
#     #' Version as \code{package_version} class.
#     getVersion = function() {
#       return(self$version)
#     },

#     #' @description
#     #' Safety function that will check if a provided group name is present in
#     #' the \code{groups} field.
#     #'
#     #' @param group_name Group name to be tested
#     checkIfGroupExists = function(group_name) {
#       if ( group_name %in% names(self$groups) == FALSE ) {
#         stop(
#           'Group `', group_name, '` not present in `groups` attribute.',
#           call. = FALSE
#         )
#       }
#     },

#     #' @description
#     #' Safety function that will check if a provided group name is present in
#     #' the meta data.
#     #'
#     #' @param group_name Group name to be tested.
#     checkIfColumnExistsInMetadata = function(group_name) {
#       if ( group_name %in% names(colData(self$expression)) == FALSE ) {
#         stop(
#           'Group `', group_name,
#           '` not present in meta data of expression data.',
#           call. = FALSE
#         )
#       }
#     },

#     #' @description
#     #' Add information to \code{experiment} field.
#     #'
#     #' @param field Name of the information, e.g. \code{organism}.
#     #' @param content Actual information, e.g. \code{hg}.
#     addExperiment = function(field, content) {
#       self$experiment[[field]] <- content
#     },

#     #' @description
#     #' Retrieve information from \code{experiment} field.
#     #'
#     #' @return
#     #' \code{list} of all entries in the \code{experiment} field.
#     getExperiment = function() {
#       return(self$experiment)
#     },

#     #' @description
#     #' Add information to \code{parameters} field.
#     #'
#     #' @param field Name of the information, e.g. \code{number_of_PCs}.
#     #' @param content Actual information, e.g. \code{30}.
#     addParameters = function(field, content) {
#       self$parameters[[field]] <- content
#     },

#     #' @description
#     #' Retrieve information from \code{parameters} field.
#     #'
#     #' @return
#     #' \code{list} of all entries in the \code{parameters} field.
#     getParameters = function() {
#       return(self$parameters)
#     },

#     #' @description
#     #' Add information to \code{technical_info} field.
#     #'
#     #' @param field Name of the information, e.g. \code{R}.
#     #' @param content Actual information, e.g. \code{4.0.2}.
#     addTechnicalInfo = function(field, content) {
#       self$technical_info[[field]] <- content
#     },

#     #' @description
#     #' Retrieve information from \code{technical_info} field.
#     #'
#     #' @return
#     #' \code{list} of all entries in the \code{technical_info} field.
#     getTechnicalInfo = function() {
#       return(self$technical_info)
#     },

#     #' @description
#     #' Add group to the groups registered in the \code{groups} field.
#     #'
#     #' @param group_name Group name.
#     #' @param levels \code{vector} of group levels (subgroups).
#     addGroup = function(group_name, levels) {
#       self$checkIfColumnExistsInMetadata(group_name)
#       self$groups[[group_name]] <- levels
#     },

#     #' @description
#     #' Retrieve all names in the \code{groups} field.
#     #'
#     #' @return
#     #' \code{vector} of registered groups.
#     getGroups = function() {
#       return(names(self$groups))
#     },

#     #' @description
#     #' Retrieve group levels for a group registered in the \code{groups} field.
#     #'
#     #' @param group_name Group name for which to retrieve group levels.
#     #'
#     #' @return
#     #' \code{vector} of group levels.
#     getGroupLevels = function(group_name) {
#       self$checkIfGroupExists(group_name)
#       self$checkIfColumnExistsInMetadata(group_name)
#       return(self$groups[[group_name]])
#     },

#     #' @description
#     #' Retrieve meta data for cells.
#     #'
#     #' @return
#     #' \code{data.frame} containing meta data.
#     getMetaData = function() {
#       return(as.data.frame(colData(self$expression)))
#     },

#     #' @description
#     #' Add a gene list to the \code{gene_lists}.
#     #'
#     #' @param name Name of the gene list.
#     #' @param \code{vector} of genes.
#     addGeneList = function(name, genes) {
#       self$gene_lists[[name]] <- genes
#     },

#     #' @description
#     #' Retrieve gene lists from the \code{gene_lists}.
#     #'
#     #' @return
#     #' \code{list} of all entries in the \code{gene_lists} field.
#     getGeneLists = function() {
#       return(self$gene_lists)
#     },

#     #' @description
#     #' Retrieve transcript count matrix.
#     #'
#     #' @return
#     #' Transcript count matrix, possibly but not necessarily in
#     #' \code{RleArray} format.
#     getExpression = function() {
#       return(SingleCellExperiment::counts(self$expression))
#     },

#     #' @description
#     #' Add columns containing cell cycle assignments to the \code{cell_cycle}
#     #' field.
#     #'
#     #' @param cols \code{vector} of columns names containing cell cycle
#     #' assignments.
#     setCellCycle = function(cols) {
#       if ( length(cols) == 1 ) {
#         self$checkIfColumnExistsInMetadata(x)
#         self$cell_cycle <- col
#       } else {
#         for ( i in seq_along(cols) ) {
#           self$checkIfColumnExistsInMetadata(cols[i])
#           self$cell_cycle <- c(self$cell_cycle, cols[i])
#         }
#       }
#     },

#     #' @description
#     #' Retrieve column names containing cell cycle assignments.
#     #'
#     #' @return
#     #' \code{vector} of column names in meta data.
#     getCellCycle = function() {
#       return(self$cell_cycle)
#     },

#     #' @description
#     #' Get list of available projections (dimensional reductions).
#     #'
#     #' @return
#     #' \code{vector} of projections / dimensional reductions that are available.
#     availableProjections = function() {
#       return(reducedDimNames(self$expression))
#     },

#     #' @description
#     #' Retrieve data for a specific projection.
#     #'
#     #' @param name Name of projection.
#     #'
#     #' @return
#     #' \code{data.frame} containing the positions of cells in the projection.
#     getProjection = function(name) {
#       if ( name %in% self$availableProjections() == FALSE ) {
#         stop(paste0('Projection "', name, '" is not available.'), call. = FALSE)
#       } else {
#         return(reducedDim(self$expression, name))
#       }
#     },

#     #' @description
#     #' Add phylogenetic tree to \code{trees} field.
#     #'
#     #' @param group_name Group name that this tree belongs to.
#     #' @param tree Phylogenetic tree as \code{phylo} object.
#     addTree = function(group_name, tree) {
#       self$checkIfGroupExists(group_name)
#       self$checkIfColumnExistsInMetadata(group_name)
#       self$trees[[group_name]] <- tree
#     },

#     #' @description
#     #' Retrieve phylogenetic tree for a specific group.
#     #'
#     #' @param group_name Group name for which to retrieve phylogenetic tree.
#     #'
#     #' @return
#     #' Phylogenetic tree as \code{phylo} object.
#     getTree = function(group_name) {
#       self$checkIfGroupExists(group_name)
#       self$checkIfColumnExistsInMetadata(group_name)
#       return(self$trees[[group_name]])
#     },

#     ## most expressed genes
#     addMostExpressedGenes = function(group_name, most_expressed_genes) {
#       self$checkIfGroupExists(group_name)
#       self$checkIfColumnExistsInMetadata(group_name)
#       self$most_expressed_genes[[group_name]] <- most_expressed_genes
#     },
#     getGroupsWithMostExpressedGenes = function() {
#       return(names(self$most_expressed_genes))
#     },
#     getMostExpressedGenes = function(group_name) {
#       self$checkIfGroupExists(group_name)
#       self$checkIfColumnExistsInMetadata(group_name)
#       return(self$most_expressed_genes[[group_name]])
#     },

#     ## marker genes
#     addMarkerGenes = function(method, group_name, content) {
#       self$checkIfGroupExists(group_name)
#       self$checkIfColumnExistsInMetadata(group_name)
#       if ( method %in% names(self$marker_genes) == FALSE ) {
#         self$marker_genes[[method]] <- list()
#       }
#       self$marker_genes[[method]][[group_name]] <- content
#     },
#     showMarkerGenes = function() {
#       text <- list()
#       for ( i in names(self$marker_genes) ) {
#         text[[i]] <- paste0(
#           '\n  - ', i, ' (', length(names(self$marker_genes[[i]])), '): ',
#           paste0(names(self$marker_genes[[i]]), collapse = ', ')
#         )
#       }
#       paste0(text, collapse = ', ')
#     },
#     getMethodsForMarkerGenes = function() {
#       return(names(self$marker_genes))
#     },
#     getGroupsWithMarkerGenes = function(method) {
#       return(names(self$marker_genes[[method]]))
#     },
#     getMarkerGenes = function(method, group_name) {
#       self$checkIfGroupExists(group_name)
#       self$checkIfColumnExistsInMetadata(group_name)
#       return(self$marker_genes[[method]][[group_name]])
#     },

#     ## enriched pathways
#     addEnrichedPathways = function(method, group_name, content) {
#       self$checkIfGroupExists(group_name)
#       self$checkIfColumnExistsInMetadata(group_name)
#       if ( method %in% names(self$enriched_pathways) == FALSE ) {
#         self$enriched_pathways[[method]] <- list()
#       }
#       self$enriched_pathways[[method]][[group_name]] <- content
#     },
#     showEnrichedPathways = function() {
#       text <- list()
#       for ( i in names(self$enriched_pathways) ) {
#         text[[i]] <- paste0(
#           '\n  - ', i, ' (', length(names(self$enriched_pathways[[i]])), '): ',
#           paste0(names(self$enriched_pathways[[i]]), collapse = ', ')
#         )
#       }
#       paste0(text, collapse = ', ')
#     },
#     getMethodsForEnrichedPathways = function() {
#       return(names(self$enriched_pathways))
#     },
#     getGroupsWithEnrichedPathways = function(method) {
#       return(names(self$enriched_pathways[[method]]))
#     },
#     getEnrichedPathways = function(method, group_name) {
#       self$checkIfGroupExists(group_name)
#       self$checkIfColumnExistsInMetadata(group_name)
#       return(self$enriched_pathways[[method]][[group_name]])
#     },

#     ## trajectories
#     addTrajectory = function(method, name, content) {
#       self$trajectories[[method]][[name]] <<- content
#     },
#     showTrajectories = function() {
#       text <- list()
#       for ( i in names(self$trajectories) ) {
#         text[[i]] <- paste0(
#           '\n  - ', i, ' (', length(names(self$trajectories[[i]])), '): ',
#           paste0(names(self$trajectories[[i]]), collapse = ', ')
#         )
#       }
#       paste0(text, collapse = ', ')
#     },
#     getMethodsForTrajectories = function() {
#       return(names(self$trajectories))
#     },
#     getNamesOfTrajectories = function(method) {
#       return(names(self$trajectories[[method]]))
#     },
#     getTrajectory = function(method, name) {
#       return(self$trajectories[[method]][[name]])
#     },

#     ## generic "print()" function to get overview of the object
#     print = function() {
#       message(
#         paste0(
#           'class: Cerebro_v1.3_SCE', '\n',
#           'cerebroApp version: ', self$getVersion(), '\n',
#           'experiment name: ', self$getExperiment()$experiment_name, '\n',
#           'organism: ', self$getExperiment()$organism, '\n',
#           'date of analysis: ', self$getExperiment()$date_of_analysis, '\n',
#           'date of export: ', self$getExperiment()$date_of_export, '\n',
#           'number of cells: ', format(ncol(self$expression), big.mark = ','), '\n',
#           'number of genes: ', format(nrow(self$expression), big.mark = ','), '\n',
#           'grouping variables (', length(self$getGroups()), '): ',
#             paste0(self$getGroups(), collapse = ', '), '\n',
#           'cell cycle variables (', length(self$cell_cycle), '): ',
#             paste0(self$cell_cycle, collapse = ', '), '\n',
#           'projections (', length(self$availableProjections()),'): ',
#             paste0(self$availableProjections(), collapse = ', '), '\n',
#           'trees (', length(self$trees),'): ',
#             paste0(names(self$trees), collapse = ', '), '\n',
#           'most expressed genes: ',
#             paste0(names(self$most_expressed_genes), collapse = ', '), '\n',
#           'marker genes:', self$showMarkerGenes(), '\n',
#           'enriched pathways:', self$showEnrichedPathways(), '\n',
#           'trajectories:', self$showTrajectories(), '\n'
#         )
#       )
#     }
#   )
# )
