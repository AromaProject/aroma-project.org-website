R.utils::use("R.utils")

scrape <- function(pathname, value=TRUE, ...) {
  bfr <- readLines(pathname, warn=FALSE)
  bfr <- paste(bfr, collapse=" ")

  items <- list()
  repeat {
    # Find opening [
    pos <- regexpr("[", bfr, fixed=TRUE)
    # Nothing more to do?
    if (pos == -1L) break
    bfr0 <- substring(bfr, first=pos-1L)

    # An image?
    if (pos > 1L) {
      bfr <- substring(bfr, first=pos-1L)
      image <- (substring(bfr, 1,1) == "!")
      bfr <- substring(bfr, first=3L)
    } else {
      image <- FALSE
      bfr <- substring(bfr, first=2L)
    }

    # Find closing ]
    pos <- regexpr("]", bfr, fixed=TRUE)
    if (pos == -1L) throw("Markdown syntax error: Link [] never closed.")
    link <- substring(bfr, first=1L, last=pos-1L)
    link <- trim(link)

    # A nested [...]?
    if (regexpr("[", link, fixed=TRUE) != -1L) {
#      if (!image) bfr0 <- substring(bfr0, first=3L)
#      bfr <- bfr0
#      mstr(bfr)
      next
    }

    # Find opening (
    bfr <- substring(bfr, first=pos+1L)
    pos <- regexpr("(", bfr, fixed=TRUE)
    # Nothing more to do?
    if (pos == -1L) break
    if (pos != 1L) break
    # Find closing )
    bfr <- substring(bfr, first=2L)
    pos <- regexpr(")", bfr, fixed=TRUE)
    if (pos == -1L) {
      throw(sprintf("Markdown syntax error: Link [%s](...) never closed: %s", link, sQuote(substring(bfr, 1, 100))))
    }
    url <- substring(bfr, first=1L, last=pos-1L)
    url <- trim(url)
    # A hidden mailing address?
##    if (grepl("mailhide", url, fixed=TRUE)) break
    bfr <- substring(bfr, first=pos+1L)

    item <- list(link=link, url=url, image=image)
    items <- c(items, list(item))
  } # repeat()
  items
} # scrape

path <- "md,trimmed"
path <- Arguments$getReadablePath(path)

pathnames <- list.files(path=path, pattern="[.]md$", full.names=TRUE, recursive=TRUE)
itemList <- lapply(pathnames, FUN=scrape)
items <- Reduce(append, itemList)

# Output references.md
md <- sapply(items, FUN=function(item) sprintf("[%s]: %s", item$link, item$url))
md <- unique(md)
md <- sort(md)
cat(md, sep="\n", file="references.md")

# Output references-by-url.md
map <- sapply(items, FUN=function(item) sprintf("%s: %s", item$url, item$link))
map <- unique(map)
map <- sort(map)
cat(map, sep="\n", file="references-by-url.md")
