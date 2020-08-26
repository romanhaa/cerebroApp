
library(rsconnect)

getOption("repos")
#                        CRAN
# "https://cran.rstudio.com/"
# attr(,"RStudio")
# [1] TRUE

setRepositories(addURLs = c(BioC = "https://bioconductor.org/packages/3.10/bioc"))

rsconnect::deployApp('~/Desktop/cerbero_hosted/', appName = 'Cerebro')
