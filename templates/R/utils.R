readLines2 <- function(file, ..., warn=FALSE, encoding=getOption("encoding")) {
  ## IMPORTANT: Must use open="rb" for input encoding to work.
  ## If one uses open="r", there will be an error! /HB 2017-01-04
  con <- file(file, open="rb", encoding=encoding)
  on.exit(close(con))

  tryCatch({
    body <- readLines(con, warn=warn, ...)
  }, warning = function(w) {
    msg <- conditionMessage(w)
    pattern <- gettextf("invalid input found on input connection")
    if (any(grepl(pattern, msg))) {
      body <- readLines(file, warn=FALSE, encoding = encoding, ...)
      mprint(con)
      msg <- sprintf("ENCODING ERROR: %s\n", msg)
      msg <- sprintf("%sInput encoding used: %s\n", msg, sQuote(encoding))
      lines <- sprintf("%3d: %s", seq_along(body), sQuote(body))
      msg <- sprintf("%sFile content parsed:\n%s\n", msg, paste(lines, collapse = "\n"))
      throw(msg)
    }
    signalCondition(w)
  })
  Encoding(body) <- encoding

  body
} # readLines2()
