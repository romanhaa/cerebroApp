setOldClass(Classes = 'package_version')

## must be initialized with expressed data as SCE object
# Cerebro_v1.3 <- setRefClass(
#   'Cerebro_v1.3',
#   fields = list(
#     version = 'package_version',
#     experiment = 'list',
#     technical_info = 'list',
#     parameters = 'list',
#     groups = 'list',
#     cell_cycle = 'vector',
#     gene_lists = 'list',
#     expression = 'SingleCellExperiment',
#     most_expressed_genes = 'list',
#     marker_genes = 'list',
#     enriched_pathways = 'list',
#     trees = 'list',
#     trajectories = 'list'
#   ),

#   ## methods to interact with the object
#   methods = list(
#     initialize = function() {
#       experiment <<- list(
#         experiment_name = NULL,
#         organism = NULL,
#         date_of_analysis = NULL,
#         date_of_export = NULL
#       )
#     },

#     ## information about version of object
#     setVersion = function(x) {
#       version <<- x
#     },
#     getVersion = function() {
#       return(version)
#     },

#     ## function for safety checks when adding data
#     checkIfGroupExists = function(group_name) {
#       if ( group_name %in% names(groups) == FALSE ) {
#         stop('Group not present in `groups` attribute.', call. = FALSE)
#       }
#     },
#     checkIfColumnExistsInMetadata = function(group_name) {
#       if ( group_name %in% names(colData(expression)) == FALSE ) {
#         stop('Group not present in meta data of expression data.', call. = FALSE)
#       }
#     },

#     ## information about experiment / data set
#     addExperiment = function(field, content) {
#       experiment[[field]] <<- content
#     },
#     getExperiment = function() {
#       return(experiment)
#     },

#     ## information about analysis parameters
#     addParameters = function(field, content) {
#       parameters[[field]] <<- content
#     },
#     getParameters = function() {
#       return(parameters)
#     },

#     ## technical information
#     addTechnicalInfo = function(field, content) {
#       technical_info[[field]] <<- content
#     },
#     getTechnicalInfo = function() {
#       return(technical_info)
#     },

#     ## grouping variables
#     setGroups = function(x) {
#       groups <<- x
#     },
#     addGroup = function(group_name, levels) {
#       checkIfColumnExistsInMetadata(group_name)
#       groups[[group_name]] <<- levels
#     },
#     getGroups = function() {
#       return(names(groups))
#     },
#     getGroupLevels = function(group_name) {
#       checkIfGroupExists(group_name)
#       checkIfColumnExistsInMetadata(group_name)
#       return(groups[[group_name]])
#     },

#     ## get data frame of cell meta data
#     getMetaData = function() {
#       return(as.data.frame(colData(expression)))
#     },

#     ## custom gene lists
#     addGeneList = function(name, genes) {
#       gene_lists[[name]] <<- genes
#     },
#     getGeneLists = function() {
#       return(gene_lists)
#     },

#     ## expression data
#     getExpression = function() {
#       return(counts(expression))
#     },

#     ## cell cycle information
#     setCellCycle = function(x) {
#       checkIfColumnExistsInMetadata(x)
#       cell_cycle <<- x
#     },
#     getCellCycle = function() {
#       return(cell_cycle)
#     },

#     ## projectionss
#     availableProjections = function() {
#       return(reducedDimNames(expression))
#     },
#     getProjection = function(name) {
#       if ( name %in% availableProjections() == FALSE ) {
#         stop(paste0('Projection "', name, '" is not available.'), call. = FALSE)
#       } else {
#         return(reducedDim(expression, name))
#       }
#     },

#     ## phylogenetic tree that shows relationship between groups of cells
#     addTree = function(group_name, tree) {
#       checkIfGroupExists(group_name)
#       checkIfColumnExistsInMetadata(group_name)
#       trees[[group_name]] <<- tree
#     },
#     getTree = function(group_name) {
#       checkIfGroupExists(group_name)
#       checkIfColumnExistsInMetadata(group_name)
#       return(trees[[group_name]])
#     },

#     ## most expressed genes
#     addMostExpressedGenes = function(group_name, most_expressed_genes) {
#       checkIfGroupExists(group_name)
#       checkIfColumnExistsInMetadata(group_name)
#       most_expressed_genes[[group_name]] <<- most_expressed_genes
#     },
#     getGroupsWithMostExpressedGenes = function() {
#       return(names(most_expressed_genes))
#     },
#     getMostExpressedGenes = function(group_name) {
#       checkIfGroupExists(group_name)
#       checkIfColumnExistsInMetadata(group_name)
#       return(most_expressed_genes[[group_name]])
#     },

#     ## marker genes
#     addMarkerGenes = function(method, group_name, content) {
#       checkIfGroupExists(group_name)
#       checkIfColumnExistsInMetadata(group_name)
#       if ( method %in% names(marker_genes) == FALSE ) {
#         marker_genes[[method]] <<- list()
#       }
#       marker_genes[[method]][[group_name]] <<- content
#     },
#     showMarkerGenes = function() {
#       p <- list()
#       for ( i in names(marker_genes) ) {
#         p[[i]] <- paste0(
#           '\n  - ', i, ' (', length(names(marker_genes[[i]])), '): ',
#           paste0(names(marker_genes[[i]]), collapse = ', ')
#         )
#       }
#       paste0(p, collapse = ', ')
#     },
#     getMethodsForMarkerGenes = function() {
#       return(names(marker_genes))
#     },
#     getGroupsWithMarkerGenes = function(method) {
#       return(names(marker_genes[[method]]))
#     },
#     getMarkerGenes = function(method, group_name) {
#       checkIfGroupExists(group_name)
#       checkIfColumnExistsInMetadata(group_name)
#       return(marker_genes[[method]][[group_name]])
#     },

#     ## enriched pathways
#     addEnrichedPathways = function(method, group_name, content) {
#       checkIfGroupExists(group_name)
#       checkIfColumnExistsInMetadata(group_name)
#       if ( method %in% names(enriched_pathways) == FALSE ) {
#         enriched_pathways[[method]] <<- list()
#       }
#       enriched_pathways[[method]][[group_name]] <<- content
#     },
#     showEnrichedPathways = function() {
#       p <- list()
#       for ( i in names(enriched_pathways) ) {
#         p[[i]] <- paste0(
#           '\n  - ', i, ' (', length(names(enriched_pathways[[i]])), '): ',
#           paste0(names(enriched_pathways[[i]]), collapse = ', ')
#         )
#       }
#       paste0(p, collapse = ', ')
#     },
#     getMethodsForEnrichedPathways = function() {
#       return(names(enriched_pathways))
#     },
#     getGroupsWithEnrichedPathways = function(method) {
#       return(names(enriched_pathways[[method]]))
#     },
#     getEnrichedPathways = function(method, group_name) {
#       checkIfGroupExists(group_name)
#       checkIfColumnExistsInMetadata(group_name)
#       return(enriched_pathways[[method]][[group_name]])
#     },

#     ## trajectories
#     addTrajectory = function(method, name, content) {
#       trajectories[[method]][[name]] <<- content
#     },
#     showTrajectories = function() {
#       p <- list()
#       for ( i in names(trajectories) ) {
#         p[[i]] <- paste0(
#           '\n  - ', i, ' (', length(names(trajectories[[i]])), '): ',
#           paste0(names(trajectories[[i]]), collapse = ', ')
#         )
#       }
#       paste0(p, collapse = ', ')
#     },
#     getMethodsForTrajectories = function() {
#       return(names(trajectories))
#     },
#     getNamesOfTrajectories = function(method) {
#       return(names(trajectories[[method]]))
#     },
#     getTrajectory = function(method, name) {
#       return(trajectories[[method]][[name]])
#     },

#     ## generic "show()" function to get overview of the object
#     show = function() {
#       message(
#         paste0(
#           'class: Cerebro_v1.3', '\n',
#           'cerebroApp version: ', .self$getVersion(), '\n',
#           'experiment name: ', .self$getExperiment()$experiment_name, '\n',
#           'organism: ', .self$getExperiment()$organism, '\n',
#           'date of analysis: ', .self$getExperiment()$date_of_analysis, '\n',
#           'date of export: ', .self$getExperiment()$date_of_export, '\n',
#           'number of cells: ', format(ncol(expression), big.mark = ','), '\n',
#           'number of genes: ', format(nrow(expression), big.mark = ','), '\n',
#           'grouping variables (', length(.self$getGroups()), '): ',
#             paste0(.self$getGroups(), collapse = ', '), '\n',
#           'cell cycle variables (', length(cell_cycle), '): ',
#             paste0(cell_cycle, collapse = ', '), '\n',
#           'projections (', length(.self$availableProjections()),'): ',
#             paste0(.self$availableProjections(), collapse = ', '), '\n',
#           'trees (', length(trees),'): ',
#             paste0(names(trees), collapse = ', '), '\n',
#           'most expressed genes: ',
#             paste0(names(most_expressed_genes), collapse = ', '), '\n',
#           'marker genes:', .self$showMarkerGenes(), '\n',
#           'enriched pathways:', .self$showEnrichedPathways(), '\n',
#           'trajectories:', .self$showTrajectories(), '\n'
#         )
#       )
#     }
#   ),
#   inheritPackage = FALSE
# )

# test <- R6Class(
#   "test",
#   list(
#     version = 'package_version',
#     experiment = 'list',
#     technical_info = 'list',
#     parameters = 'list',
#     groups = 'list',
#     cell_cycle = 'vector',
#     gene_lists = 'list',
#     expression = 'SingleCellExperiment',
#     most_expressed_genes = 'list',
#     marker_genes = 'list',
#     enriched_pathways = 'list',
#     trees = 'list',
#     trajectories = 'list',

#     ##
#     initialize = function() {
#       self$experiment <- list(
#         experiment_name = NULL,
#         organism = NULL,
#         date_of_analysis = NULL,
#         date_of_export = NULL
#       )
#     },

#     ##
#     print = function(
#       ...
#     ) {
#       cat("Person: \n")
#       cat("  Name: ", self$name, "\n", sep = "")
#       cat("  Age:  ", self$age, "\n", sep = "")
#       invisible(self)
#     },

#     ##
#     getExperiment = function() {
#       return(self$experiment)
#     }

#   )
# )

Cerebro_v1.3 <- R6::R6Class(
  'Cerebro_v1.3',

  ##--------------------------------------------------------------------------##
  ## fields/slots in the object
  ##--------------------------------------------------------------------------##

  list(
    version = c(),
    experiment = list(),
    technical_info = list(),
    parameters = list(),
    groups = list(),
    cell_cycle = c(),
    gene_lists = list(),
    expression = SingleCellExperiment(),
    most_expressed_genes = list(),
    marker_genes = list(),
    enriched_pathways = list(),
    trees = list(),
    trajectories = list(),

    ##------------------------------------------------------------------------##
    ## methods to interact with the object
    ##------------------------------------------------------------------------##

    ## initialize object
    initialize = function() {
      self$experiment <- list(
        experiment_name = NULL,
        organism = NULL,
        date_of_analysis = NULL,
        date_of_export = NULL
      )
    },

    ## information about version of object
    setVersion = function(x) {
      self$version <- x
    },
    getVersion = function() {
      return(self$version)
    },

    ## function for safety checks when adding data
    checkIfGroupExists = function(group_name) {
      if ( group_name %in% names(self$groups) == FALSE ) {
        stop('Group not present in `groups` attribute.', call. = FALSE)
      }
    },
    checkIfColumnExistsInMetadata = function(group_name) {
      if ( group_name %in% names(colData(self$expression)) == FALSE ) {
        stop('Group not present in meta data of expression data.', call. = FALSE)
      }
    },

    ## information about experiment / data set
    addExperiment = function(field, content) {
      self$experiment[[field]] <- content
    },
    getExperiment = function() {
      return(self$experiment)
    },

    ## information about analysis parameters
    addParameters = function(field, content) {
      self$parameters[[field]] <- content
    },
    getParameters = function() {
      return(self$parameters)
    },

    ## technical information
    addTechnicalInfo = function(field, content) {
      self$technical_info[[field]] <- content
    },
    getTechnicalInfo = function() {
      return(self$technical_info)
    },

    ## grouping variables
    setGroups = function(x) {
      self$groups <- x
    },
    addGroup = function(group_name, levels) {
      self$checkIfColumnExistsInMetadata(group_name)
      self$groups[[group_name]] <- levels
    },
    getGroups = function() {
      return(names(self$groups))
    },
    getGroupLevels = function(group_name) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      return(self$groups[[group_name]])
    },

    ## get data frame of cell meta data
    getMetaData = function() {
      return(as.data.frame(colData(self$expression)))
    },

    ## custom gene lists
    addGeneList = function(name, genes) {
      self$gene_lists[[name]] <- genes
    },
    getGeneLists = function() {
      return(self$gene_lists)
    },

    ## expression data
    getExpression = function() {
      return(SingleCellExperiment::counts(self$expression))
    },

    ## cell cycle information
    setCellCycle = function(x) {
      self$checkIfColumnExistsInMetadata(x)
      self$cell_cycle <- x
    },
    getCellCycle = function() {
      return(self$cell_cycle)
    },

    ## projectionss
    availableProjections = function() {
      return(reducedDimNames(self$expression))
    },
    getProjection = function(name) {
      if ( name %in% self$availableProjections() == FALSE ) {
        stop(paste0('Projection "', name, '" is not available.'), call. = FALSE)
      } else {
        return(reducedDim(self$expression, name))
      }
    },

    ## phylogenetic tree that shows relationship between groups of cells
    addTree = function(group_name, tree) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      self$trees[[group_name]] <- tree
    },
    getTree = function(group_name) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      return(self$trees[[group_name]])
    },

    ## most expressed genes
    addMostExpressedGenes = function(group_name, most_expressed_genes) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      self$most_expressed_genes[[group_name]] <- most_expressed_genes
    },
    getGroupsWithMostExpressedGenes = function() {
      return(names(self$most_expressed_genes))
    },
    getMostExpressedGenes = function(group_name) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      return(self$most_expressed_genes[[group_name]])
    },

    ## marker genes
    addMarkerGenes = function(method, group_name, content) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      if ( method %in% names(self$marker_genes) == FALSE ) {
        self$marker_genes[[method]] <- list()
      }
      self$marker_genes[[method]][[group_name]] <- content
    },
    showMarkerGenes = function() {
      text <- list()
      for ( i in names(self$marker_genes) ) {
        text[[i]] <- paste0(
          '\n  - ', i, ' (', length(names(self$marker_genes[[i]])), '): ',
          paste0(names(self$marker_genes[[i]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },
    getMethodsForMarkerGenes = function() {
      return(names(self$marker_genes))
    },
    getGroupsWithMarkerGenes = function(method) {
      return(names(self$marker_genes[[method]]))
    },
    getMarkerGenes = function(method, group_name) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      return(self$marker_genes[[method]][[group_name]])
    },

    ## enriched pathways
    addEnrichedPathways = function(method, group_name, content) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      if ( method %in% names(self$enriched_pathways) == FALSE ) {
        self$enriched_pathways[[method]] <- list()
      }
      self$enriched_pathways[[method]][[group_name]] <- content
    },
    showEnrichedPathways = function() {
      text <- list()
      for ( i in names(self$enriched_pathways) ) {
        text[[i]] <- paste0(
          '\n  - ', i, ' (', length(names(self$enriched_pathways[[i]])), '): ',
          paste0(names(self$enriched_pathways[[i]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },
    getMethodsForEnrichedPathways = function() {
      return(names(self$enriched_pathways))
    },
    getGroupsWithEnrichedPathways = function(method) {
      return(names(self$enriched_pathways[[method]]))
    },
    getEnrichedPathways = function(method, group_name) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      return(self$enriched_pathways[[method]][[group_name]])
    },

    ## trajectories
    addTrajectory = function(method, name, content) {
      self$trajectories[[method]][[name]] <<- content
    },
    showTrajectories = function() {
      text <- list()
      for ( i in names(self$trajectories) ) {
        text[[i]] <- paste0(
          '\n  - ', i, ' (', length(names(self$trajectories[[i]])), '): ',
          paste0(names(self$trajectories[[i]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },
    getMethodsForTrajectories = function() {
      return(names(self$trajectories))
    },
    getNamesOfTrajectories = function(method) {
      return(names(self$trajectories[[method]]))
    },
    getTrajectory = function(method, name) {
      return(self$trajectories[[method]][[name]])
    },

    ## generic "print()" function to get overview of the object
    print = function() {
      message(
        paste0(
          'class: Cerebro_v1.3', '\n',
          'cerebroApp version: ', self$getVersion(), '\n',
          'experiment name: ', self$getExperiment()$experiment_name, '\n',
          'organism: ', self$getExperiment()$organism, '\n',
          'date of analysis: ', self$getExperiment()$date_of_analysis, '\n',
          'date of export: ', self$getExperiment()$date_of_export, '\n',
          'number of cells: ', format(ncol(self$expression), big.mark = ','), '\n',
          'number of genes: ', format(nrow(self$expression), big.mark = ','), '\n',
          'grouping variables (', length(self$getGroups()), '): ',
            paste0(self$getGroups(), collapse = ', '), '\n',
          'cell cycle variables (', length(self$cell_cycle), '): ',
            paste0(self$cell_cycle, collapse = ', '), '\n',
          'projections (', length(self$availableProjections()),'): ',
            paste0(self$availableProjections(), collapse = ', '), '\n',
          'trees (', length(self$trees),'): ',
            paste0(names(self$trees), collapse = ', '), '\n',
          'most expressed genes: ',
            paste0(names(self$most_expressed_genes), collapse = ', '), '\n',
          'marker genes:', self$showMarkerGenes(), '\n',
          'enriched pathways:', self$showEnrichedPathways(), '\n',
          'trajectories:', self$showTrajectories(), '\n'
        )
      )
    }
  )
)
