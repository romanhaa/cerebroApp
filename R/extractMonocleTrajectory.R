#' Extract trajectory from Monocle and add to Seurat object.
#' @title Extract trajectory from Monocle and add to Seurat object.
#' @description This function takes a Monocle object, extracts a trajectory that
#' was calculated, and stores it in the specified Seurat object. Trajectory info
#' (state, pseudotime, projection and tree) will be stored in
#' seurat@misc$trajectories under the specified name.
#' @keywords Cerebro scRNAseq Seurat Monocle trajectory
#' @param monocle Monocle object to extract trajectory from.
#' @param seurat Seurat object to transfer trajectory to.
#' @param trajectory_name Name of trajectory.
#' @param column_state Name of meta data column that holds info about the state
#' of a cell; defaults to 'State'.
#' @param column_pseudotime Name of meta data column that holds info about the
#' pseudotime of a cell; defaults to 'Pseudotime'.
#' @export
#' @return Returns Seurat object with added trajectory. Trajectory info (state,
#' pseudotime, projection and tree) will be stored in
#' `object@misc$trajectories$monocle2` under the specified name.
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' seurat <- extractMonocleTrajectory(
#'   monocle = monocle,
#'   seurat = seurat,
#'   name = 'trajectory_1',
#'   column_state = 'State',
#'   column_pseudotime = 'Pseudotime'
#' )
#' }
extractMonocleTrajectory <- function(
  monocle,
  seurat,
  trajectory_name,
  column_state = 'State',
  column_pseudotime = 'Pseudotime'
) {

  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  ## check if Seurat is installed
  if ( !requireNamespace("Seurat", quietly = TRUE) ) {
    stop(
      "The 'Seurat' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## check that Seurat package is at least v3.0
  if ( utils::packageVersion('Seurat') < 3 ) {
    stop(
      paste0(
        "The installed Seurat package is of version `", utils::packageVersion('Seurat'),
        "`, but at least v3.0 is required."
      ),
      call. = FALSE
    )
  }

  ## check if monocle is installed
  if ( !requireNamespace("monocle", quietly = TRUE) ) {
    stop(
      "The 'monocle' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## check if provided Seurat object is of class "Seurat"
  if ( class(seurat) != "Seurat" ) {
    stop(
      paste0(
        "Provided object is of class `", class(seurat), "` but must be of class 'Seurat'."
      ),
      call. = FALSE
    )
  }

  ## check version of Seurat object and stop if it is lower than 3
  if ( seurat@version < 3 ) {
    stop(
      paste0(
        "Provided Seurat object has version `", seurat@version, "` but must be at least 3.0."
      ),
      call. = FALSE
    )
  }

  ## check if provided monocle object is of class "CellDataSet"
  if ( !methods::is(monocle, 'CellDataSet') ) {
    stop(
      "The provided object for 'monocle' is not of type 'CellDataSet'.",
      call. = FALSE
    )
  }

  ## check if "column_state" exists in monocle object
  if ( (column_state %in% colnames(monocle@phenoData@data)) == FALSE ) {
    stop(
      paste0(
        "Specified column for state info ('", column_state,
        "') could not be found in meta data."
      ),
      call. = FALSE
    )
  }

  ## check if "column_pseudotime" exists in monocle object
  if ( (column_pseudotime %in% colnames(monocle@phenoData@data)) == FALSE ) {
    stop(
      paste0(
        "Specified column for pseudotime info ('", column_pseudotime,
        "') could not be found in meta data."
      ),
      call. = FALSE
    )
  }

  ## check if "minSpanningTree" exists in monocle object
  if ( length(monocle@minSpanningTree) == 0 ) {
    stop(
      'monocle@minSpanningTree appears to be empty but is required.',
      call. = FALSE
    )
  }

  ## check if "reducedDimS" exists in monocle object
  if ( length(monocle@reducedDimS) == 0 ) {
    stop(
      'monocle@reducedDimS appears to be empty but is required.',
      call. = FALSE
    )
  }

  ## check if "reducedDimK" exists in monocle object
  if ( length(monocle@reducedDimK) == 0 ) {
    stop(
      'monocle@reducedDimK appears to be empty but is required.',
      call. = FALSE
    )
  }

  ## check if a trajectory with name "trajectory_name" already exists in the
  ## Seurat object
  if ( !is.null(seurat@misc$trajectories[[trajectory_name]]) ) {
    stop(
      paste0(
        "Trajectory with specified name ('", trajectory_name,
        "') already exists in seurat@misc$trajectories. Please choose a ',
        'different name or manually remove data from that slot."
      ),
      call. = FALSE
    )
  }

  ## check if the monocle object contains more cells than the Seurat object
  ## (monocle object is only allowed to contain the same cells or a subset)
  if ( nrow(monocle@phenoData@data) > nrow(seurat@meta.data) ) {
    stop(
      paste0(
        'Number of cells in monocle object (', nrow(monocle@phenoData@data),
        ') cannot be larger than number of cells in Seurat object (',
        nrow(seurat@meta.data), ').'
      ),
      call. = FALSE
    )
  }

  ## check if monocle object contains cells that are not present in the Seurat
  ## object
  if (
    length(which(rownames(monocle@phenoData@data) %in%
    rownames(seurat@meta.data))) != nrow(monocle@phenoData@data) )
  {
    stop(
      paste0(
        'Some cells provided in the Monocle object are not present in the ',
        'Seurat object. This is not supported. Please re-calculate the ',
        'trajectory with all or a subset of the cells present in the Seurat ',
        'object.'
      ),
      call. = FALSE
    )
  }

  ## check if trajectory in monocle object is two-dimensional
  if ( nrow(monocle@reducedDimK) > 2 ) {
    stop(
      'Currently only two-dimensional reductions are supported.',
      call. = FALSE
    )
  }

  ## check if monocle object contains a subset of cells in the Seurat object and
  ## print warning if that is the case
  if ( nrow(monocle@phenoData@data) < nrow(seurat@meta.data) ) {
    warning(
      paste0(
        'There are ', nrow(seurat@meta.data) - nrow(monocle@phenoData@data),
        ' cells present in the Seurat object but not in the Monocle object. ',
        'Cells without trajectory information will not be visible in Cerebro.'
      ),
      call. = FALSE
    )
  }

  ##--------------------------------------------------------------------------##
  ## Prepare reducedDimK.
  ##--------------------------------------------------------------------------##

  temp_reducedDimK <- monocle@reducedDimK %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename(dim_1 = 1, dim_2 = 2)

  reduced_dim_K <- temp_reducedDimK %>%
    dplyr::mutate(sample_name = rownames(temp_reducedDimK))

  ##--------------------------------------------------------------------------##
  ## Transform trajectory into plottable edges.
  ##--------------------------------------------------------------------------##

  edges <- monocle@minSpanningTree %>%
    igraph::as_data_frame() %>%
    dplyr::rename(source = 'from', target = 'to')

  edges <- dplyr::left_join(
      edges,
      reduced_dim_K %>% dplyr::rename(
        source = 'sample_name',
        source_dim_1 = 'dim_1',
        source_dim_2 = 'dim_2'
      ),
      by = 'source'
    )

  edges <- dplyr::left_join(
      edges,
      reduced_dim_K %>% dplyr::rename(
        target = 'sample_name',
        target_dim_1 = 'dim_1',
        target_dim_2 = 'dim_2'
      ),
      by = 'target'
    ) %>%
    dplyr::select(c('source','target','weight','source_dim_1','source_dim_2',
                    'target_dim_1','target_dim_2'))

  ##--------------------------------------------------------------------------##
  ## Extract state and pseudotime info and cell position in projection.
  ##--------------------------------------------------------------------------##

  trajectory_meta_from_monocle <- data.frame(
    cell = rownames(monocle@phenoData@data),
    pseudotime = monocle@phenoData@data[[column_pseudotime]],
    state = monocle@phenoData@data[[column_state]],
    row.names = rownames(monocle@phenoData@data),
    stringsAsFactors = FALSE
  )

  temp_reducedDimS <- monocle@reducedDimS %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename(DR_1 = 1, DR_2 = 2)

  trajectory_info <- temp_reducedDimS %>%
    dplyr::mutate(cell = rownames(temp_reducedDimS))

  trajectory_info <- dplyr::left_join(
      trajectory_info,
      trajectory_meta_from_monocle,
      by = 'cell'
    )

  trajectory_info <- dplyr::left_join(
      seurat@meta.data %>% dplyr::mutate(cell = rownames(seurat@meta.data)),
      trajectory_info,
      by = 'cell'
    ) %>%
    dplyr::select(c('DR_1','DR_2','pseudotime','state','cell'))

  rownames(trajectory_info) <- trajectory_info$cell
  trajectory_info <- trajectory_info %>% dplyr::select(-'cell')

  ##--------------------------------------------------------------------------##
  ## Add trajectory info to Seurat object.
  ##--------------------------------------------------------------------------##

  if ( is.null(seurat@misc$trajectories) ) {
    seurat@misc$trajectories <- list()
  }
  seurat@misc$trajectories$monocle2[[trajectory_name]] <- list(
    meta = trajectory_info,
    edges = edges
  )

  ##--------------------------------------------------------------------------##
  ## return Seurat object
  ##--------------------------------------------------------------------------##
  return(seurat)
}
