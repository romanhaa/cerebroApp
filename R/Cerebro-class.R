setOldClass(Classes = 'package_version')

## muste be initialized with expressed data as SCE object
Cerebro <- setRefClass(
  'Cerebro',
  fields = list(
    version = 'package_version',
    experiment = 'list',
    technical_info = 'list',
    parameters = 'list',
    groups = 'list',
    cell_cycle = 'vector',
    gene_lists = 'list',
    expression = 'SingleCellExperiment',
    most_expressed_genes = 'list',
    marker_genes = 'list',
    enriched_pathways = 'list',
    trees = 'list',
    trajectories = 'list'
  ),
  #contains = c('SingleCellExperiment'),
  methods = list(
    initialize = function()
    {
      experiment <<- list(
        experiment_name = NULL,
        organism = NULL,
        date_of_analysis = NULL,
        date_of_export = NULL
      )
    },
    ## information about version of object
    setVersion = function(x)
    {
      version <<- x
    },
    getVersion = function()
    {
      return(version)
    },
    ## information about experiment / data set
    addExperiment = function(field, content)
    {
      experiment[[field]] <<- content
    },
    getExperiment = function()
    {
      return(experiment)
    },
    ## technical information
    addTechnicalInfo = function(field, content)
    {
      technical_info[[field]] <<- content
    },
    getTechnicalInfo = function()
    {
      return(technical_info)
    },
    ## information about analysis parameters
    addParameters = function(field, content)
    {
      parameters[[field]] <<- content
    },
    getParameters = function()
    {
      return(parameters)
    },
    ## names of variables that cells can be grouped by, e.g. sample, cluster
    setGroups = function(x)
    {
      groups <<- x
    },
    getGroups = function()
    {
      return(names(groups))
    },
    checkIfGroupExists = function(group_name)
    {
      if ( group_name %in% names(groups) == FALSE ) {
        stop('Group not present in `groups` attribute.', call. = FALSE)
      }
    },
    checkIfColumnExistsInMetadata = function(group_name)
    {
      if ( group_name %in% names(colData(expression)) == FALSE ) {
        stop('Group not present in meta data of expression data.', call. = FALSE)
      }
    },
    addGroup = function(group_name, levels)
    {
      checkIfColumnExistsInMetadata(group_name)
      groups[[group_name]] <<- levels
    },
    getGroupLevels = function(group_name)
    {
      checkIfGroupExists(group_name)
      checkIfColumnExistsInMetadata(group_name)
      return(groups[[group_name]])
    },
    ## cell cycle information
    setCellCycle = function(x)
    {
      checkIfColumnExistsInMetadata(x)
      cell_cycle <<- x
    },
    ## custom gene lists
    addGeneList = function(name, genes)
    {
      gene_lists[[name]] <<- genes
    },
    ## expression data
    getExpression = function()
    {
      return(assay(expression, 'expression'))
    },
    ## most expressed genes
    addMostExpressedGenes = function(group_name, most_expressed_genes)
    {
      checkIfGroupExists(group_name)
      checkIfColumnExistsInMetadata(group_name)
      most_expressed_genes[[group_name]] <<- most_expressed_genes
    },
    getGroupsWithMostExpressedGenes = function()
    {
      return(names(most_expressed_genes))
    },
    getMostExpressedGenes = function(group_name)
    {
      checkIfGroupExists(group_name)
      checkIfColumnExistsInMetadata(group_name)
      return(most_expressed_genes[[group_name]])
    },
    ## marker genes
    addMarkerGenes = function(method, group_name, content)
    {
      checkIfGroupExists(group_name)
      checkIfColumnExistsInMetadata(group_name)
      if ( method %in% names(marker_genes) == FALSE )
      {
        marker_genes[[method]] <<- list()
      }
      marker_genes[[method]][[group_name]] <<- content
    },
    showMarkerGenes = function()
    {
      p <- list()
      for ( i in names(marker_genes) )
      {
        p[[i]] <- paste0(
          i, ' (',
          paste0(names(marker_genes[[i]]), collapse = ', '), ')'
        )
      }
      paste0(p, collapse = ', ')
    },
    getMethodsForMarkerGenes = function()
    {
      return(names(marker_genes))
    },
    getGroupsWithMarkerGenes = function(method)
    {
      return(names(marker_genes[[method]]))
    },
    getMarkerGenes = function(method, group_name)
    {
      checkIfGroupExists(group_name)
      checkIfColumnExistsInMetadata(group_name)
      return(marker_genes[[method]][[group_name]])
    },
    ## enriched pathways
    addEnrichedPathways = function(method, group_name, content)
    {
      checkIfGroupExists(group_name)
      checkIfColumnExistsInMetadata(group_name)
      if ( method %in% names(enriched_pathways) == FALSE )
      {
        enriched_pathways[[method]] <<- list()
      }
      enriched_pathways[[method]][[group_name]] <<- content
    },
    showEnrichedPathways = function()
    {
      p <- list()
      for ( i in names(enriched_pathways) )
      {
        p[[i]] <- paste0(
          i, ' (',
          paste0(names(enriched_pathways[[i]]), collapse = ', '), ')'
        )
      }
      paste0(p, collapse = ', ')
    },
    getMethodsForEnrichedPathways = function()
    {
      return(names(enriched_pathways))
    },
    getGroupsWithEnrichedPathways = function(method)
    {
      return(names(enriched_pathways[[method]]))
    },
    getEnrichedPathways = function(method, group_name)
    {
      checkIfGroupExists(group_name)
      checkIfColumnExistsInMetadata(group_name)
      return(enriched_pathways[[method]][[group_name]])
    },
    ## phylogenetic tree that shows relationship between groups of cells
    addTree = function(group_name, tree)
    {
      checkIfGroupExists(group_name)
      checkIfColumnExistsInMetadata(group_name)
      trees[[group_name]] <<- tree
    },
    getTree = function(group_name)
    {
      checkIfGroupExists(group_name)
      checkIfColumnExistsInMetadata(group_name)
      return(trees[[group_name]])
    },
    ## trajectories
    addTrajectory = function(method, name, content)
    {
      trajectories[[method]][[name]] <<- content
    },
    showTrajectories = function()
    {
      p <- list()
      for ( i in names(trajectories) )
      {
        p[[i]] <- paste0(
          i, ' (',
          paste0(names(trajectories[[i]]), collapse = ', '), ')'
        )
      }
      paste0(p, collapse = ', ')
    },
    getTrajectory = function(method, name)
    {
      return(trajectories[[method]][[name]])
    },
    ## generic "show()" function to get overview of the object
    show = function()
    {
      message(
        paste0(
          'class: Cerebro', '\n',
          'version: ',
            .self$getVersion(), '\n',
          'experiment name: ',
            .self$getExperiment()$experiment_name, '\n',
          'organism: ',
            .self$getExperiment()$organism, '\n',
          'date of analysis: ',
            .self$getExperiment()$date_of_analysis, '\n',
          'date of export: ',
            .self$getExperiment()$date_of_export, '\n',
          'number of cells: ',
            format(ncol(expression), big.mark = ','), '\n',
          'number of genes: ',
            format(nrow(expression), big.mark = ','), '\n',
          'grouping variables (', length(.self$getGroups()), '): ',
            paste0(.self$getGroups(), collapse = ', '), '\n',
          'cell cycle variables (', length(cell_cycle), '): ',
            paste0(cell_cycle, collapse = ', '), '\n',
          'projections (', length(reducedDims(expression)),'): ',
            paste0(names(reducedDims(expression)), collapse = ', '), '\n',
          'trees (', length(trees),'): ',
            paste0(names(trees), collapse = ', '), '\n',
          'most expressed genes: ',
            paste0(names(most_expressed_genes), collapse = ', '), '\n',
          'marker genes: ',
            .self$showMarkerGenes(), '\n',
          'enriched pathways: ',
            .self$showEnrichedPathways(), '\n',
          'trajectories (', length(trajectories) ,'): ',
            .self$showTrajectories(), '\n'
        )
      )
    }
  ),
  inheritPackage = FALSE
)
