##----------------------------------------------------------------------------##
## Tab: Trajectory
##----------------------------------------------------------------------------##

source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/trajectory/select_method_and_name.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/trajectory/projection.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/trajectory/selected_cells_table.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/trajectory/distribution_along_pseudotime.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/trajectory/states_by_group.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/trajectory/expression_metrics.R"), local = TRUE)
