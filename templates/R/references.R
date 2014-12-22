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


get_cite <- function(key, ...) {
  if (!is.element(key, names(references))) {
    throw(sprintf("Reference '%s' not found: %s", key,
          paste(sQuote(sort(names(references))), collapse=", ")))
  }
  references[[key]]
}

citep <- function(key, ...) {
  bib <- get_cite(key)
  cat(Citep(bib))
}

citet <- function(key, ...) {
  bib <- get_cite(key)
  cat(Citet(bib))
}

nocite <- function(key, ...) {
  bib <- get_cite(key)
  NoCite(bib)
}

bibentry <- function(ref, key=NULL, keywords=FALSE, crossref=TRUE, style="markdown") {
  oopts <- BibOptions(style=style)
  on.exit(BibOptions(oopts))
  md <- format(ref, style=style)

  if (!crossref) {
    if (style == "html") {
      if (any(grepl("<cite>", md, fixed=TRUE))) {
        md <- gsub(".*<cite>(|\n)*", "", md, fixed=FALSE)
      }
    }
  }

  if (style == "html") {
    ## Drop stray </cite>?
    if (!any(grepl("<cite>", md, fixed=TRUE))) {
      md <- gsub("</cite>", "", md, fixed=TRUE)
    }
  }

  ## Drop [1] at the beginning; we roll our own
  md <- sub("\\[1\\]( |\\n)", "", md)
  mprint(md)
  mcat('--------\n\n')
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
  oopts <- BibOptions(hyperlink=FALSE, style=style)
  on.exit(BibOptions(oopts))
  cat('<div class="alert alert-info" role="alert">\n')
  cat(' <p>\n')
  cat('  <span class="glyphicon glyphicon-thumbs-up" style="font-size: 1.2em;"></span>\n')
  cat('  <em>To help support this work, please consider citing the following relevant references in your publications or talks whenever using their methods or results:</em>\n')
  cat(' </p><br>\n')
  cat(' <ul>\n')
  for (key in keys) {
    cat('  <li>\n')
    bib <- references[[key]]
    bibentry(bib, keywords=keywords, crossref=FALSE, ..., style=style)
    cat('  </li>\n')
  }
  cat(' </ul>\n')
  cat('</div>\n')
} # please_cite()

biblist <- function(.opts=list(check.entries = FALSE, sorting = "ynt"), ...) {
  PrintBibliography(references, .opts=.opts, ...)
} # biblist()


alert_warn <- function(expr, ..., envir=parent.frame(), style="html") {
  oopts <- BibOptions(style=style)
  on.exit(BibOptions(oopts))
  cat('<div class="alert alert-warning" role="alert">\n')
  cat('  <span class="glyphicon glyphicon-exclamation-sign" style="font-size: 1.2em;"></span>\n')
  eval(expr, envir=envir)
  cat('</div>\n')
} # alert_warn()
