#' @title
#' Gene enrichment using Enrichr.
#'
#' @description
#' Gene enrichment using Enrichr.
#'
#' @param genes Gene names or dataframe of gene names in first column and a
#' score between 0 and 1 in the other.
#' @param databases Databases to search.
#' @param URL_API URL to send requests to (Enrichr API).
#' See https://maayanlab.cloud/Enrichr/#stats for available databases.
#'
#' @return
#' Returns a data frame of enrichment terms, p-values, ...
#'
#' @importFrom httr GET POST
#'
#' @author Wajid Jawaid, modified by Roman Hillje
#'
.send_enrichr_query <- function(
  genes,
  databases = NULL,
  URL_API = NULL
) {

  ## check input format
  ## ... input is a vector with length > 0 and not all empty genes
  if (
    is.vector(genes) &&
    length(genes) > 0 &&
    !all(genes == '')
  ) {

    ## send request with gene names
    temp <- httr::POST(
      url = paste0(URL_API, "/enrich"),
      body = list(
        list = paste(genes, collapse = '\n')
      )
    )

  ## ... input is a data frame
  } else if ( is.data.frame(genes) ) {

    ## send request with gene names and scores
    temp <- httr::POST(
      url = paste0(URL_API, "/enrich"),
      body = list(
        list = paste(paste(genes[,1], genes[,2], sep = ','), collapse = '\n')
      )
    )

  ## ... none of the above
  } else {

    warning(
      paste0('genes must be a non-empty vector of gene names or a dataframe ',
      'with genes and score.'
      )
    )
  }

  ## 
  httr::GET(url = paste0(URL_API, "/share"))

  ##
  dfSAF <- options()$stringsAsFactors

  ## 
  options()

  ##
  result <- future.apply::future_sapply(
    databases,
    USE.NAMES = TRUE,
    simplify = FALSE,
    function(x)
  {

    ##
    r <- httr::GET(
      url = paste0(URL_API, "/export"),
      query = list(
        file = 'API',
        backgroundType = x
      )
    )

    ##
    r <- gsub('&#39;', "'", intToUtf8(r$content))

    ##
    tc <- textConnection(r)

    ##
    r <- utils::read.table(
      tc,
      sep = '\t',
      header = TRUE,
      quote = '',
      comment.char = '',
      stringsAsFactors = FALSE
    )

    ##
    close(tc)

    ##
    return(r)
  })

  ##
  return(result)
}
