##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##

source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/groups/select_group.R"), local = TRUE)
source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/groups/tree.R"), local = TRUE)
source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/groups/composition.R"), local = TRUE)
source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/groups/number_of_transcripts.R"), local = TRUE)
source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/groups/number_of_expressed_genes.R"), local = TRUE)
source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/groups/mitochondrial_gene_expression.R"), local = TRUE)
source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/groups/ribosomal_gene_expression.R"), local = TRUE)
source(paste0(.GlobalEnv$Cerebro.options[["path_to_shiny_files"]], "/groups/cell_cycle.R"), local = TRUE)
