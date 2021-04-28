#' @title test
#' @description Functions to find columns of specific type (for automatic formatting).
#' @param table test
#' @param groupA test
#' @param groupB test
#' @param mode test
#' @param percent test
#' @return test
.findColumnsPValues <- function(df) {
  pattern_columns_p_value <- "pval|p_val|p-val|p.val|padj|p_adj|p-adj|p.adj|adjp|adj_p|adj-p|adj.p|FDR|qval|q_val|q-val|q.val"
  columns_indices <- c()
  for ( i in 1:ncol(df) ) {
    if (
      grepl(colnames(df)[i], pattern = pattern_columns_p_value, ignore.case = TRUE) &&
      any(is.na(df[[i]])) == FALSE &&
      is.numeric(df[[i]]) &&
      min(df[[i]], na.rm = TRUE) >= 0 &&
      max(df[[i]], na.rm = TRUE) <= 1
    ) {
      columns_indices <- c(columns_indices, i)
    }
  }
  return(columns_indices)
}
