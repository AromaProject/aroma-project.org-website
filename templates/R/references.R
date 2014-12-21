R.utils::use("R.utils")
use("RefManageR")
BibOptions(check.entries=FALSE)

## WORKAROUND:
## Argument 'style' is not fully acknowledged
## when calling format() below.
BibOptions(style="markdown")

## Load references
references <- ReadBib("assets/references/references.bib")

## Sort
references <- sort(references, sorting="ydnt")


citep <- function(key, ...) {
  bib <- references[[key]]
  mprint(bib)
  cat(Citep(bib))
}

citet <- function(key, ...) {
  bib <- references[[key]]
  cat(Citet(bib))
}

nocite <- function(key, ...) {
  bib <- references[[key]]
  NoCite(bib)
}

bibentry <- function(ref, key=NULL, keywords=FALSE, style="markdown") {
  oopts <- BibOptions(style=style)
  on.exit(BibOptions(oopts))
  md <- format(ref, style=style)
  md <- sub("\\[1\\]( |\\n)", "", md)
  md <- gsub("..", ".", md, fixed=TRUE)
  if (keywords) {
    keywords <- ref$keywords
    if (!is.null(keywords)) {
      keywords <- trim(unlist(strsplit(keywords, split="[,;]")))
      md <- sprintf("%s  \nKeywords: %s", md, paste(keywords, collapse="; "))
    }
  }
  md <- sprintf("%s\n\n", md)
  if (!is.null(key)) md <- sprintf("[%s] %s", kk, md)
  cat(md)
} # bibentry()

please_cite <- function(keys, keywords=FALSE, ..., style="html") {
  cat('<div class="alert alert-info" role="alert">\n')
  cat(' <p>\n')
  cat('  <span class="glyphicon glyphicon-thumbs-up" style="font-size: 1.2em;"></span>\n')
  cat('  <em>To help support this work, please consider citing one or more of the following references in your publications or talks when using the methods on this page:</em>\n')
  cat(' </p><br>\n')
  cat(' <ul>\n')
  for (key in keys) {
    cat('  <li>\n')
    bib <- references[[key]]
    bibentry(bib, keywords=keywords, ..., style=style)
    cat('  </li>\n')
  }
  cat(' </ul>\n')
  cat('</div>\n')
} # please_cite()

biblist <- function(.opts=list(check.entries = FALSE, sorting = "ynt"), ...) {
  PrintBibliography(references, .opts=.opts, ...)
} # biblist()

