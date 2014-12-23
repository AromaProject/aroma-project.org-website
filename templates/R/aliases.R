chipTypeData <- function(chipType, filename) {
  url <- sprintf("http://aroma-project.org/data/annotationData/chipTypes/%s/%s", chipType, filename)
  sprintf("[%s](%s)", filename, url)
} # chipTypeData()


cran <- function(name) {
  printf("[%s](http://cran.r-project.org/package=%s)", name, name)
}

bioc <- function(name) {
  if (grepl("^(pd|org|BSgenome)[.]", name) || grepl("([.]db|[.]db0|cdf|probe)$", name)) {
    printf("[%s](http://www.bioconductor.org/packages/release/data/annotation/html/%s.html)", name, name)
  } else {
    printf("[%s](http://www.bioconductor.org/packages/release/bioc/html/%s.html)", name, name)
  }
}

pmid <- function(id) {
  printf("[%s](http://www.ncbi.nlm.nih.gov/pubmed/%s)", id, id)
}

pcmid <- function(id) {
  printf("[%s](http://www.ncbi.nlm.nih.gov/pmc/articles/%s/)", id, id)
}

geo <- function(id) {
  printf("[%s](http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s)", id, id)
}

arrayexpress <- function(id) {
  printf("[%s](http://www.ebi.ac.uk/microarray-as/ae/browse.html?keywords=%s)", id, id)
}
