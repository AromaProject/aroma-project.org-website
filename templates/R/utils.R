readLines2 <- function(file, ..., warn=FALSE, encoding=getOption("encoding")) {
  con <- file(file, open="r", encoding=encoding)
  on.exit(close(con))

  body <- readLines(con, warn=warn, ...)
  Encoding(body) <- encoding

  body
} # readLines2()
