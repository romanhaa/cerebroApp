#' @title test
#' @description Calculate X-Y ranges for projections.
#' @param df test
#' @return test
.getXYranges <- function(df) {
  ranges <- list(
    x = list(
      min = df[,1] %>% min(na.rm=TRUE) %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round(),
      max = df[,1] %>% max(na.rm=TRUE) %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
    ),
    y = list(
      min = df[,2] %>% min(na.rm=TRUE) %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round(),
      max = df[,2] %>% max(na.rm=TRUE) %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
    )
  )
  return(ranges)
}
