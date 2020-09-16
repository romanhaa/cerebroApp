setOldClass(Classes = 'package_version')

#' @title
#' R6 class in which data sets will be stored for visualization in Cerebro.
#'
#' @description
#' A \code{Cerebro_v1.3} object is an R6 class that contains several types of
#' data that can be visualized in Cerebro.
#' 
#' @return
#' A new \code{Cerebro_v1.3} object.
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
Cerebro_v1.3 <- R6::R6Class(
  'Cerebro_v1.3',

  ## public fields and methods
  public = list(

    #' @field version cerebroApp version that was used to create the object.
    version = c(),

    #' @field experiment \code{list} that contains meta data about the data set,
    #' including experiment name, species, date of export.
    experiment = list(),

    #' @field technical_info \code{list} that contains technical information
    #' about the analysis, including the R session info.
    technical_info = list(),

    #' @field parameters \code{list} that contains important parameters that
    #' were used during the analysis, e.g. cut-off values for cell filtering.
    parameters = list(),

    #' @field groups \code{list} that contains specified grouping variables and
    #' and the group levels (subgroups) that belong to each of them. For each
    #' grouping variable, a corresponding column with the same name must exist
    #' in the meta data.
    groups = list(),

    #' @field cell_cycle \code{vector} that contains the name of columns in the
    #' meta data that contain cell cycle assignments.
    cell_cycle = c(),

    #' @field gene_lists \code{list} that contains gene lists, e.g.
    #' mitochondrial and/or ribosomal genes.
    gene_lists = list(),

    #' @field expression \code{matrix}-like object that holds transcript counts.
    expression = NULL,

    #' @field meta_data \code{data.frame} that contains cell meta data.
    meta_data = data.frame(),

    #' @field projections \code{list} that contains projections/dimensional
    #' reductions.
    projections = list(),

    #' @field most_expressed_genes \code{list} that contains a \code{data.frame}
    #' holding the most expressed genes for each grouping variable that was
    #' specified during the call to \code{\link{getMostExpressedGenes}}.
    most_expressed_genes = list(),

    #' @field marker_genes \code{list} that contains a \code{list} for every
    #' method that was used to calculate marker genes, and a \code{data.frame}
    #' for each grouping variable, e.g. those that were specified during the
    #' call to \code{\link{getMarkerGenes}}.
    marker_genes = list(),

    #' @field enriched_pathways \code{list} that contains a \code{list} for
    #' every method that was used to calculate marker genes, and a
    #' \code{data.frame} for each grouping variable, e.g. those that were
    #' specified during the call to \code{\link{getEnrichedPathways}} or
    #' \code{\link{performGeneSetEnrichmentAnalysis}}.
    enriched_pathways = list(),

    #' @field trees \code{list} that contains a phylogenetic tree (class
    #' \code{phylo}) for grouping variables.
    trees = list(),

    #' @field trajectories \code{list} that contains a \code{list} for every
    #' method that was used to calculate trajectories, and, depending on the
    #' method, a \code{data.frame} or \code{list} for each specific trajectory,
    #' e.g. those extracted with \code{\link{extractMonocleTrajectory}}.
    trajectories = list(),

    #' @field extra_material \code{list} that can contain additional material
    #' related to the data set; tables should be stored in \code{data.frame}
    #' format in a named \code{list} called `tables`
    extra_material = list(),

    ##------------------------------------------------------------------------##
    ## methods to interact with the object
    ##------------------------------------------------------------------------##

    #' @description
    #' Create a new \code{Cerebro_v1.3} object.
    #'
    #' @return
    #' A new \code{Cerebro_v1.3} object.
    initialize = function() {
      self$experiment <- list(
        experiment_name = NULL,
        organism = NULL,
        date_of_analysis = NULL,
        date_of_export = NULL
      )
    },

    #' @description
    #' Set the version of \code{cerebroApp} that was used to generate this
    #' object.
    #'
    #' @param version Version to set.
    setVersion = function(version) {
      self$version <- version
    },

    #' @description
    #' Get the version of \code{cerebroApp} that was used to generate this
    #' object.
    #' 
    #' @return
    #' Version as \code{package_version} class.
    getVersion = function() {
      return(self$version)
    },

    #' @description
    #' Safety function that will check if a provided group name is present in
    #' the \code{groups} field.
    #'
    #' @param group_name Group name to be tested
    checkIfGroupExists = function(group_name) {
      if ( group_name %in% names(self$groups) == FALSE ) {
        stop(
          glue::glue('Group `{group_name}` not present in `groups` attribute.'),
          call. = FALSE
        )
      }
    },

    #' @description
    #' Safety function that will check if a provided group name is present in
    #' the meta data.
    #'
    #' @param group_name Group name to be tested.
    checkIfColumnExistsInMetadata = function(group_name) {
      if ( group_name %in% colnames(self$meta_data) == FALSE ) {
        stop(
          glue::glue('Group `{group_name}` not present in meta data.'),
          call. = FALSE
        )
      }
    },

    #' @description
    #' Add information to \code{experiment} field.
    #'
    #' @param field Name of the information, e.g. \code{organism}.
    #' @param content Actual information, e.g. \code{hg}.
    addExperiment = function(field, content) {
      self$experiment[[field]] <- content
    },

    #' @description
    #' Retrieve information from \code{experiment} field.
    #'
    #' @return
    #' \code{list} of all entries in the \code{experiment} field.
    getExperiment = function() {
      return(self$experiment)
    },

    #' @description
    #' Add information to \code{parameters} field.
    #'
    #' @param field Name of the information, e.g. \code{number_of_PCs}.
    #' @param content Actual information, e.g. \code{30}.
    addParameters = function(field, content) {
      self$parameters[[field]] <- content
    },

    #' @description
    #' Retrieve information from \code{parameters} field.
    #'
    #' @return
    #' \code{list} of all entries in the \code{parameters} field.
    getParameters = function() {
      return(self$parameters)
    },

    #' @description
    #' Add information to \code{technical_info} field.
    #'
    #' @param field Name of the information, e.g. \code{R}.
    #' @param content Actual information, e.g. \code{4.0.2}.
    addTechnicalInfo = function(field, content) {
      self$technical_info[[field]] <- content
    },

    #' @description
    #' Retrieve information from \code{technical_info} field.
    #'
    #' @return
    #' \code{list} of all entries in the \code{technical_info} field.
    getTechnicalInfo = function() {
      return(self$technical_info)
    },

    #' @description
    #' Add group to the groups registered in the \code{groups} field.
    #'
    #' @param group_name Group name.
    #' @param levels \code{vector} of group levels (subgroups).
    addGroup = function(group_name, levels) {
      self$checkIfColumnExistsInMetadata(group_name)
      self$groups[[group_name]] <- levels
    },

    #' @description
    #' Retrieve all names in the \code{groups} field.
    #'
    #' @return
    #' \code{vector} of registered groups.
    getGroups = function() {
      return(names(self$groups))
    },

    #' @description
    #' Retrieve group levels for a group registered in the \code{groups} field.
    #'
    #' @param group_name Group name for which to retrieve group levels.
    #'
    #' @return
    #' \code{vector} of group levels.
    getGroupLevels = function(group_name) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      return(self$groups[[group_name]])
    },

    #' @description
    #' Set meta data for cells.
    #'
    #' @param table \code{data.frame} that contains meta data for cells. The
    #' number of rows must be equal to the number of rows of projections and
    #' the number of columns in the transcript count matrix.
    setMetaData = function(table) {
      ## TODO: add checks for nrow() and type of input
      self$meta_data <- table
    },

    #' @description
    #' Retrieve meta data for cells.
    #'
    #' @return
    #' \code{data.frame} containing meta data.
    getMetaData = function() {
      return(self$meta_data)
    },

    #' @description
    #' Add a gene list to the \code{gene_lists}.
    #'
    #' @param name Name of the gene list.
    #' @param genes \code{vector} of genes.
    addGeneList = function(name, genes) {
      self$gene_lists[[name]] <- genes
    },

    #' @description
    #' Retrieve gene lists from the \code{gene_lists}.
    #'
    #' @return
    #' \code{list} of all entries in the \code{gene_lists} field.
    getGeneLists = function() {
      return(self$gene_lists)
    },

    #' @description
    #' Set transcript count matrix.
    #'
    #' @param counts \code{matrix}-like object that contains transcript counts
    #' for cells in the data set. Number of columns must be equal to the number
    #' of rows in the \code{meta_data} field.
    setExpression = function(counts) {
      ## TODO: check type?
      self$expression <- counts
    },

    #' @description
    #' Get names of all cells.
    #'
    #' @return
    #' \code{vector} containing all cell names/barcodes.
    getCellNames = function() {
      return(colnames(self$expression))
    },

    #' @description
    #' Get names of all genes in transcript count matrix.
    #'
    #' @return
    #' \code{vector} containing all gene names in transcript count matrix.
    getGeneNames = function() {
      return(rownames(self$expression))
    },


    #' @description
    #' Retrieve mean expression across all cells in the data set for a set of
    #' genes.
    #'
    #' @param genes Names of genes to extract; no default.
    #'
    #' @return
    #' \code{data.frame} containing specified gene names and their respective
    #' mean expression across all cells in the data set.
    getMeanExpressionForGenes = function(genes) {

      ## check what kind of matrix the transcription counts are stored as
      ## ... DelayedArray / RleMatrix
      if ( class(self$expression) == 'RleMatrix' ) {

        ## get indices of specified genes
        gene_indices <- match(genes, rownames(self$expression))

        ## calculate mean expression per gene
        mean_expression <- Matrix::rowMeans(
          DelayedArray::extract_array(
            self$expression,
            list(gene_indices, NULL)
          )
        )

      ## ... anything else
      } else {

        ## calculate mean expression per gene
        mean_expression <- Matrix::rowMeans(self$expression[genes , , drop = FALSE])
      }

      ##
      return(
        data.frame(
          "gene" = genes,
          "expression" = mean_expression
        )
      )
    },

    #' @description
    #' Retrieve (mean) expression for a single gene or a set of genes for a
    #' given set of cells.
    #'
    #' @param cells Names/barcodes of cells to extract; defaults to \code{NULL},
    #' which will return all cells.
    #' @param genes Names of genes to extract; defaults to \code{NULL}, which
    #' will return all genes.
    #'
    #' @return
    #' \code{vector} containing (mean) expression across all specified genes in
    #' each specified cell.
    getMeanExpressionForCells = function(cells = NULL, genes = NULL) {

      ## check what kind of matrix the transcription counts are stored as
      ## ... DelayedArray / RleMatrix
      if ( class(self$expression) == 'RleMatrix' ) {

        ## if cell names were provided, get their indices
        if (
          !is.null(cells) &&
          is.character(cells)
        ) {
          cell_names <- cells
          cell_indices <- match(cells, colnames(self$expression))
        } else if ( is.null(cells) ) {
          cell_names <- colnames(self$expression)
          cell_indices <- NULL
        }

        ## if gene names were provided, get their indices
        if (
          !is.null(genes) &&
          is.character(genes)
        ) {
          gene_names <- genes
          gene_indices <- match(genes, rownames(self$expression))
        } else if ( is.null(genes) ) {
          gene_names <- rownames(self$expression)
          gene_indices <- NULL
        }

        ## extract (dense) matrix of requested cells and genes and make sure it
        ## stays in matrix format, even if it has only a single row or column
        mean_expression <- Matrix::colMeans(
          DelayedArray::extract_array(
            self$expression,
            list(gene_indices, cell_indices)
          )
        )

      ## ... anything else
      } else {

        ## if cell names were not provided, extract names of all cells
        if ( is.null(cells) ) {
          cells <- colnames(self$expression)
        }

        ## if genes names were not provided, extract names of all genes
        if ( is.null(genes) ) {
          genes <- rownames(self$expression)
        }

        ## return (dense) matrix for requested cells and genes and make sure it
        ## stays in matrix format, even if it has only a single row or column
        mean_expression <- Matrix::colMeans(self$expression[genes, cells, drop = FALSE])
      }

      ##
      return(mean_expression)
    },

    #' @description
    #' Retrieve transcript count matrix.
    #'
    #' @param cells Names/barcodes of cells to extract; defaults to \code{NULL},
    #' which will return all cells.
    #' @param genes Names of genes to extract; defaults to \code{NULL}, which
    #' will return all genes.
    #'
    #' @return
    #' Dense transcript count matrix for specified cells and genes.
    getExpressionMatrix = function(cells = NULL, genes = NULL) {

      ## check what kind of matrix the transcription counts are stored as
      ## ... DelayedArray / RleMatrix
      if ( class(self$expression) == 'RleMatrix' ) {

        ## if cell names were provided, get their indices
        if (
          !is.null(cells) &&
          is.character(cells)
        ) {
          cell_names <- cells
          cell_indices <- match(cells, colnames(self$expression))
        } else if ( is.null(cells) ) {
          cell_names <- colnames(self$expression)
          cell_indices <- NULL
        }

        ## if gene names were provided, get their indices
        if (
          !is.null(genes) &&
          is.character(genes)
        ) {
          gene_names <- genes
          gene_indices <- match(genes, rownames(self$expression))
        } else if ( is.null(genes) ) {
          gene_names <- rownames(self$expression)
          gene_indices <- NULL
        }

        ## extract (dense) matrix of requested cells and genes and make sure it
        ## stays in matrix format, even if it has only a single row or column
        matrix <- as.matrix(
          DelayedArray::extract_array(
            self$expression,
            list(gene_indices, cell_indices)
          )
        )

        ## assign column and row names
        colnames(matrix) <- cell_names
        rownames(matrix) <- gene_names

        ## return matrix
        return(matrix)

      ## ... anything else
      } else {

        ## if cell names were not provided, extract names of all cells
        if ( is.null(cells) ) {
          cells <- colnames(self$expression)
        }

        ## if genes names were not provided, extract names of all genes
        if ( is.null(genes) ) {
          genes <- rownames(self$expression)
        }

        ## return (dense) matrix for requested cells and genes and make sure it
        ## stays in matrix format, even if it has only a single row or column
        return(
          as.matrix(
            self$expression[genes, cells, drop = FALSE]
          )
        )
      }
    },

    #' @description
    #' Add columns containing cell cycle assignments to the \code{cell_cycle}
    #' field.
    #'
    #' @param cols \code{vector} of columns names containing cell cycle
    #' assignments.
    setCellCycle = function(cols) {
      if ( length(cols) == 1 ) {
        self$checkIfColumnExistsInMetadata(cols)
        self$cell_cycle <- cols
      } else {
        for ( i in seq_along(cols) ) {
          self$checkIfColumnExistsInMetadata(cols[i])
          self$cell_cycle <- c(self$cell_cycle, cols[i])
        }
      }
    },

    #' @description
    #' Retrieve column names containing cell cycle assignments.
    #'
    #' @return
    #' \code{vector} of column names in meta data.
    getCellCycle = function() {
      return(self$cell_cycle)
    },

    #' @description
    #' Add projections (dimensional reductions).
    #'
    #' @param name Name of the projection.
    #' @param projection \code{data.frame} containing positions of cells in
    #' projection.
    addProjection = function(name, projection) {
      # ## check if projection with same name already exists
      # if ( name %in% names(self$projections) ) {
      #   stop(
      #     glue::glue(
      #       'A projection with the name `{name}` already exists. ',
      #       'Please use a different name.'
      #     ),
      #     call. = FALSE
      #   )
      # }
      # ## check if provided projection is a data frame
      # if ( is.data.frame(projection) == FALSE ) {
      #   stop(
      #     glue::glue(
      #       'Provided projection is of type `{class(projection)}` but should ',
      #       'be a data frame. Please convert it.'
      #     ),
      #     call. = FALSE
      #   )
      # }
      ## TODO: check dimensions?
      self$projections[[name]] <- projection
    },

    #' @description
    #' Get list of available projections (dimensional reductions).
    #'
    #' @return
    #' \code{vector} of projections / dimensional reductions that are available.
    availableProjections = function() {
      return(names(self$projections))
    },

    #' @description
    #' Retrieve data for a specific projection.
    #'
    #' @param name Name of projection.
    #'
    #' @return
    #' \code{data.frame} containing the positions of cells in the projection.
    getProjection = function(name) {
      if ( name %in% self$availableProjections() == FALSE ) {
        stop(glue::glue('Projection `{name}` is not available.'), call. = FALSE)
      } else {
        return(self$projections[[name]])
      }
    },

    #' @description
    #' Add phylogenetic tree to \code{trees} field.
    #'
    #' @param group_name Group name that this tree belongs to.
    #' @param tree Phylogenetic tree as \code{phylo} object.
    addTree = function(group_name, tree) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      self$trees[[group_name]] <- tree
    },

    #' @description
    #' Retrieve phylogenetic tree for a specific group.
    #'
    #' @param group_name Group name for which to retrieve phylogenetic tree.
    #'
    #' @return
    #' Phylogenetic tree as \code{phylo} object.
    getTree = function(group_name) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      return(self$trees[[group_name]])
    },

    #' @description
    #' Add table of most expressed genes.
    #'
    #' @param group_name Name of grouping variable that the most expressed genes
    #' belong to. Must be registered in the \code{groups} field.
    #' @param table \code{data.frame} that contains the most expressed genes.
    addMostExpressedGenes = function(group_name, table) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      self$most_expressed_genes[[group_name]] <- table
    },

    #' @description
    #' Retrieve names of grouping variables for which most expressed genes are
    #' available.
    #'
    #' @return
    #' \code{vector} of grouping variables for which most expressed genes are
    #' available.
    getGroupsWithMostExpressedGenes = function() {
      return(names(self$most_expressed_genes))
    },

    #' @description
    #' Retrieve table of most expressed genes for a grouping variable.
    #'
    #' @param group_name Grouping variable for which most expressed genes should
    #' be retrieved.
    #'
    #' @return
    #' \code{data.frame} that contains most expressed genes for group levels of
    #' the specified grouping variable.
    getMostExpressedGenes = function(group_name) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      return(self$most_expressed_genes[[group_name]])
    },

    #' @description
    #' Add table of marker genes.
    #'
    #' @param method Name of method that was used to generate the marker genes.
    #' @param name Name of table. This name will be used to select the table in
    #' Cerebro. It is recommended to use the grouping variable, e.g.
    #' \code{sample}.
    #' @param table \code{data.frame} that contains the marker genes.
    addMarkerGenes = function(method, name, table) {
      # self$checkIfGroupExists(group_name)
      # self$checkIfColumnExistsInMetadata(group_name)
      if ( method %in% names(self$marker_genes) == FALSE ) {
        self$marker_genes[[method]] <- list()
      }
      self$marker_genes[[method]][[name]] <- table
    },

    #' @description
    #' Retrieve names of methods that were used to generate marker genes.
    #'
    #' @return
    #' \code{vector} of names of methods that were used to generate marker
    #' genes.
    getMethodsForMarkerGenes = function() {
      return(names(self$marker_genes))
    },

    #' @description
    #' Retrieve grouping variables for which marker genes were generated using
    #' a specified method.
    #'
    #' @param method Name of method.
    #'
    #' @return
    #' \code{vector} of grouping variables for which marker genes were
    #' calculated using the specified method.
    getGroupsWithMarkerGenes = function(method) {
      return(names(self$marker_genes[[method]]))
    },

    #' @description
    #' Retrieve table of marker genes for specific method and grouping variable.
    #'
    #' @param method Name of method.
    #' @param name Name of table.
    #'
    #' @return
    #' \code{data.frame} that contains marker genes for the specified
    #' combination of method and grouping variable.
    getMarkerGenes = function(method, name) {
      # self$checkIfGroupExists(group_name)
      # self$checkIfColumnExistsInMetadata(group_name)
      if ( method %in% names(self$marker_genes) == FALSE ) {
        stop(glue::glue('Method `{method}` is not available for marker genes.'), call. = FALSE)
      }
      if ( name %in% names(self$marker_genes[[method]]) == FALSE ) {
        stop(glue::glue('A marker gene table with name `{name}` is not available for method `{method}`.'), call. = FALSE)
      }
      return(self$marker_genes[[method]][[name]])
    },

    #' @description
    #' Add table of enriched pathways.
    #'
    #' @param method Name of method that was used to generate the enriched
    #' pathways.
    #' @param name Name of table. This name will be used to select the table in
    #' Cerebro. It is recommended to use the grouping variable, e.g.
    #' \code{sample}.
    #' @param table \code{data.frame} that contains the enriched pathways.
    addEnrichedPathways = function(method, name, table) {
      # self$checkIfGroupExists(group_name)
      # self$checkIfColumnExistsInMetadata(group_name)
      if ( method %in% names(self$enriched_pathways) == FALSE ) {
        self$enriched_pathways[[method]] <- list()
      }
      self$enriched_pathways[[method]][[name]] <- table
    },

    #' @description
    #' Retrieve names of methods that were used to generate enriched pathways.
    #'
    #' @return
    #' \code{vector} of names of methods that were used to generate enriched
    #' pathways.
    getMethodsForEnrichedPathways = function() {
      return(names(self$enriched_pathways))
    },

    #' @description
    #' Retrieve grouping variables for which enriched pathways were generated
    #' using a specified method.
    #'
    #' @param method Name of method.
    #'
    #' @return
    #' \code{vector} of grouping variables for which enriched pathways were
    #' calculated using the specified method.
    getGroupsWithEnrichedPathways = function(method) {
      return(names(self$enriched_pathways[[method]]))
    },

    #' @description
    #' Retrieve table of enriched pathways for specific method and grouping
    #' variable.
    #'
    #' @param method Name of method.
    #' @param name Grouping variable.
    #'
    #' @return
    #' \code{data.frame} that contains enriched pathways for the specified
    #' combination of method and grouping variable.
    getEnrichedPathways = function(method, name) {
      # self$checkIfGroupExists(group_name)
      # self$checkIfColumnExistsInMetadata(group_name)
      if ( method %in% names(self$enriched_pathways) == FALSE ) {
        stop(glue::glue('Method `{method}` is not available for enriched pathways.'), call. = FALSE)
      }
      if ( name %in% names(self$enriched_pathways[[method]]) == FALSE ) {
        stop(glue::glue('A pathway enrichment table with name `{name}` is not available for method `{method}`.'), call. = FALSE)
      }
      return(self$enriched_pathways[[method]][[name]])
    },

    #' @description
    #' Add trajectory.
    #'
    #' @param method Name of method that was used to generate the trajectory.
    #' @param name Name of the trajectory. This name will be used later in
    #' Cerebro to select the trajectory.
    #' @param content Relevant data for the trajectory, depending on the method
    #' this could be a \code{list} holding edges, cell positions, pseudotime,
    #' etc.
    addTrajectory = function(method, name, content) {
      self$trajectories[[method]][[name]] <- content
    },

    #' @description
    #' Retrieve names of methods that were used to generate trajectories.
    #'
    #' @return
    #' \code{vector} of names of methods that were used to generate
    #' trajectories.
    getMethodsForTrajectories = function() {
      return(names(self$trajectories))
    },

    #' @description
    #' Retrieve names of available trajectories for a specified method.
    #'
    #' @param method Name of method.
    #'
    #' @return
    #' \code{vector} of available trajectory for the specified method.
    getNamesOfTrajectories = function(method) {
      return(names(self$trajectories[[method]]))
    },

    #' @description
    #' Retrieve data for a specific trajectory.
    #'
    #' @param method Name of method.
    #' @param name Name of trajectory.
    #'
    #' @return
    #' The type of data depends on the method that was used to generate the
    #' trajectory.
    getTrajectory = function(method, name) {
      return(self$trajectories[[method]][[name]])
    },

    #' @description
    #' Add content to extra material field.
    #'
    #' @param category Name of category. At the moment, only \code{tables} is a
    #' valid category.
    #' @param name Name of material, will be used to select it in Cerebro.
    #' @param content Data that should be added.
    addExtraMaterial = function(category, name, content) {

      ## valid categories
      valid_categories <- c('tables')

      ## proceed only if specified category is valid
      if ( category %in% valid_categories == FALSE ) {
        stop(
          glue::glue(
            'Category `{category}` is not one of the valid categories ',
            '({paste0(valid_categories, collapse = ", ")}).'
          ),
          call. = FALSE
        )
      }

      ## call function to add table
      if ( category == 'tables' ) {
        self$addExtraTable(name, content)
      }
    },

    #' @description
    #' Add table to `extra_material` slot.
    #' 
    #' @param name Name of material, will be used to select it in Cerebro.
    #' @param table Table that should be added, must be \code{data.frame}.
    addExtraTable = function(name, table) {

      ## stop if table is not a data frame
      if ( !is.data.frame(table) ) {
        stop(
          glue::glue(
            'Cannot add table `{table}` because it is not a data frame.'
          ),
          call. = FALSE
        )

      ## stop if `name` is already used
      } else if (
        !is.null(self$extra_material) &&
        !is.null(self$extra_material$tables) &&
        is.list(self$extra_material$tables) &&
        name %in% names(self$extra_material$tables)
      ) {
        stop(
          glue::glue(
            'A table with name `{name}` already exists in the extra material. '
          ),
          call. = FALSE
        )

      ## add table
      } else {
        self$extra_material$tables[[ name ]] <- table
      }
    },

    #' @description
    #' Get names of categories for which extra material is available.
    #'
    #' @return
    #' \code{vector} with names of available categories.
    getExtraMaterialCategories = function() {
      return(names(self$extra_material))
    },

    #' @description
    #' Check whether there are tables in the extra materials.
    #'
    #' @return
    #' \code{logical} indicating whether there are tables in the extra
    #' materials.
    checkForExtraTables = function() {
      return(!is.null(self$extra_material$tables))
    },

    #' @description
    #' Get names of tables in extra materials.
    #'
    #' @return
    #' \code{vector} containing names of tables in extra materials.
    getNamesOfExtraTables = function() {
      return(names(self$extra_material$tables))
    },

    #' @description
    #' Get table from extra materials.
    #'
    #' @param name Name of table.
    #'
    #' @return
    #' Requested table in \code{data.frame} format.
    getExtraTable = function(name) {
      return(self$extra_material$table[[ name ]])
    },

    #' @description
    #' Show overview of object and the data it contains.
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
          'marker genes:', private$showMarkerGenes(), '\n',
          'enriched pathways:', private$showEnrichedPathways(), '\n',
          'trajectories:', private$showTrajectories(), '\n',
          'extra material:', private$showExtraMaterial(), '\n'
        )
      )
    }
  ),

  ## private fields and methods
  private = list(

    #' Print overview of available marker gene results for \code{self$print()}
    #' function.
    showMarkerGenes = function() {
      text <- list()
      for ( method in names(self$marker_genes) ) {
        text[[method]] <- paste0(
          '\n  - ', method, ' (', length(names(self$marker_genes[[method]])), '): ',
          paste0(names(self$marker_genes[[method]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },

    #' Print overview of available enriched pathway results for
    #' \code{self$print()} function.
    showEnrichedPathways = function() {
      text <- list()
      for ( method in names(self$enriched_pathways) ) {
        text[[method]] <- paste0(
          '\n  - ', method, ' (', length(names(self$enriched_pathways[[method]])), '): ',
          paste0(names(self$enriched_pathways[[method]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },

    #' Print overview of available trajectories for \code{self$print()} function.
    showTrajectories = function() {
      text <- list()
      for ( method in names(self$trajectories) ) {
        text[[method]] <- paste0(
          '\n  - ', method, ' (', length(names(self$trajectories[[method]])), '): ',
          paste0(names(self$trajectories[[method]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },

    #' Print overview of extra material for \code{self$print()} function.
    showExtraMaterial = function() {
      text <- list()
      for ( category in names(self$extra_material) ) {
        text[[category]] <- paste0(
          '\n  - ', category, ' (', length(names(self$extra_material[[category]])), '): ',
          paste0(names(self$extra_material[[category]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    }
  )
)
