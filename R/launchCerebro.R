#' Launch Cerebro interface.
#' @title Launch Cerebro interface.
#' @description Launch Cerebro interface.
#' @keywords Cerebro scRNAseq Seurat
#' @param version Which version of Cerebro to launch: "v1.0", "v1.1", "v1.2",
#' "v1.3"; defaults to "v1.3".
#' @param ... Further parameters that are used by the specific versions of
#' Cerebro. See `launchCerebroV1.x()` for details.
#' @export
#' @return Shiny application.
#' @examples
#' if ( interactive() ) {
#'   launchCerebro(version = "v1.3")
#' }
launchCerebro <- function(
  version = "v1.3",
  ...
){

  ##--------------------------------------------------------------------------##
  ## Check input parameters.
  ##--------------------------------------------------------------------------##
  available_versions <- c("v1.0","v1.1","v1.2","v1.3")

  if ( (version %in% available_versions == FALSE ) )
  {
    stop(
      'Version must be one of: ', paste0(available_versions, collapse = ', '),
      call. = FALSE
    )
  }

  ##--------------------------------------------------------------------------##
  ## Launch Cerebro.
  ##--------------------------------------------------------------------------##
  if ( version == "v1.0" ) {
    launchCerebroV1.0(...)
  } else if ( version == "v1.1" ) {
    launchCerebroV1.1(...)
  } else if ( version == "v1.2" ) {
    launchCerebroV1.2(...)
  } else if ( version == "v1.3" ) {
    launchCerebroV1.3(...)
  }
}
