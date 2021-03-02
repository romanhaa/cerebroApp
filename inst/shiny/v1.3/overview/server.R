##----------------------------------------------------------------------------##
## Tab: Overview
##----------------------------------------------------------------------------##
files_to_load <- list.files(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/overview"),
  pattern = "func_|obj_|UI_|out_|event_",
  full.names = TRUE
)

for ( i in files_to_load ) {
  source(i, local = TRUE)
}
