#' Gene enrichment using Enrichr.
#' @title Gene enrichment using Enrichr.
#' @description Gene enrichment using Enrichr.
#' @param genes Gene names or dataframe of gene names in first column and a
#' score between 0 and 1 in the other.
#' @param databases Databases to search.
#' @param API_url URL to send requests to (Enrichr API).
#' See http://amp.pharm.mssm.edu/Enrichr/ for available databases.
#' @return Returns a data frame of enrichment terms, p-values, ...
#' @author Wajid Jawaid, modified by Roman Hillje
send_enrichr_query <- function(
  genes,
  databases = NULL,
  URL_API = NULL
)
{
  if (is.vector(genes) & ! all(genes == '') & length(genes) != 0) {
    temp <- httr::POST(
      url = URL_API,
      body = list(list = paste(genes, collapse = '\n'))
    )
  } else if (is.data.frame(genes)) {
    temp <- httr::POST(
      url = URL_API,
      body = list(list = paste(paste(genes[,1], genes[,2], sep = ','), collapse = '\n'))
    )
  } else {
    warning('genes must be a non-empty vector of gene names or a dataframe with genes and score.')
  }
  httr::GET(url = 'http://amp.pharm.mssm.edu/Enrichr/share')
  dfSAF <- options()$stringsAsFactors
  options(stringsAsFactors = FALSE)
  result <- future.apply::future_sapply(databases, USE.NAMES = TRUE, simplify = FALSE, function(x) {
    r <- httr::GET(
      url = 'http://amp.pharm.mssm.edu/Enrichr/export',
      query = list(file = 'API', backgroundType = x)
    )
    r <- gsub('&#39;', "'", intToUtf8(r$content))
    tc <- textConnection(r)
    r <- read.table(tc, sep = '\t', header = TRUE, quote = '', comment.char = '')
    close(tc)
    return(r)
  })
  options(stringsAsFactors = dfSAF)
  return(result)
}
