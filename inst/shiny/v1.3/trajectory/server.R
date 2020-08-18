##----------------------------------------------------------------------------##
## Tab: Trajectory
##----------------------------------------------------------------------------##

# TODO: let user show gene expression as well? probably more complicated

source(paste0(path_to_shiny_files, "/trajectory/select_method_and_name.R"), local = TRUE)
source(paste0(path_to_shiny_files, "/trajectory/projection.R"), local = TRUE)
source(paste0(path_to_shiny_files, "/trajectory/selected_cells_table.R"), local = TRUE)
source(paste0(path_to_shiny_files, "/trajectory/distribution_along_pseudotime.R"), local = TRUE)
source(paste0(path_to_shiny_files, "/trajectory/states_by_group.R"), local = TRUE)
source(paste0(path_to_shiny_files, "/trajectory/number_of_transcripts_by_state.R"), local = TRUE)
source(paste0(path_to_shiny_files, "/trajectory/number_of_expressed_genes_by_state.R"), local = TRUE)
