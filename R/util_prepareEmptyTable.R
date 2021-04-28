#' @title test
#' @description Function to prepare empty table.
#' @param table test
#' @return test
.prepareEmptyTable <- function(table) {
  DT::datatable(
    table,
    autoHideNavigation = TRUE,
    class = "stripe table-bordered table-condensed",
    escape = FALSE,
    filter = "none",
    rownames = FALSE,
    selection = "none",
    style = "bootstrap",
    options = list(
      buttons = list(),
      dom = "Brtip",
      lengthMenu = c(20, 50, 100),
      pageLength = 20,
      scrollX = TRUE
    )
  )
}
