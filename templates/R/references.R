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

cite_patch <- function(x) {
  stopifnot(length(x) == 1L)

  ## PATCH: & in URLs should be encoded as &amp;

  # Markdown or HTML anchors (or neither)?
  if (grepl('\\]\\([a-z]+://', x)) {
    pattern <- '.*\\]\\([a-z]+://[^?]*[?]([^)]+)\\).*'
  } else if (grepl('href=["\'][a-z]+://', x)) {
    pattern <- '.*href=["\'][a-z]+://[^?]*[?]([^"\']+)[^"\'].*'
  } else {
    # Nothing todo?
    return(x)
  }
  if (grepl(pattern, x, ignore.case=TRUE)) {
    url <- gsub(pattern, "\\1", x, ignore.case=TRUE)
    url2 <- gsub("&amp;", "<<<AMP>>>", url, fixed=TRUE)
    url2 <- gsub("&", "&amp;", url2, fixed=TRUE)
    url2 <- gsub("<<<AMP>>>", "&amp;", url2, fixed=TRUE)
    x <- gsub(url, url2, x, fixed=TRUE)
  }
  x
}

citep <- function(key, ...) {
  bib <- get_cite(key)
  cat(cite_patch(Citep(bib)))
}

citet <- function(key, ...) {
  bib <- get_cite(key)
  cat(cite_patch(Citet(bib)))
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
        md <- gsub("<p>.*<cite>(|\n)*", "<p>", md, fixed=FALSE)
      }
    }
  }

  if (style == "html") {
    ## Drop stray </cite>? (from above?)
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

  md <- cite_patch(md)

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

biblist_clear <- function() {
  cites_env <- RefManageR:::.cites
  cites_env$indices <- logical(0L)
  cites_env$labs <- character(0L)
} # biblist_clear()

biblist <- function(.opts=list(check.entries = FALSE, sorting = "ynt"), clear=TRUE, ...) {
  PrintBibliography(references, .opts=.opts, ...)
  if (clear) biblist_clear()
} # biblist()


alert_warn <- function(expr, ..., envir=parent.frame(), style="html") {
  oopts <- BibOptions(style=style)
  on.exit(BibOptions(oopts))
  cat('<div class="alert alert-warning" role="alert">\n')
  cat('  <span class="glyphicon glyphicon-exclamation-sign" style="font-size: 1.2em;"></span>\n')
  eval(expr, envir=envir)
  cat('</div>\n')
} # alert_warn()

alert_help <- function(expr, ..., envir=parent.frame(), style="html") {
  oopts <- BibOptions(style=style)
  on.exit(BibOptions(oopts))
  cat('<div class="alert alert-info" role="alert">\n')
  cat('  <span class="glyphicon glyphicon-flag" style="font-size: 1.2em;"></span>\n')
  eval(expr, envir=envir)
  cat('</div>\n')
} # alert_help()


alert_info <- function(expr, ..., icon="info-sign", css=NULL, envir=parent.frame(), style="html") {
  oopts <- BibOptions(style=style)
  on.exit(BibOptions(oopts))
  if (!is.null(css)) {
    printf('<div class="alert alert-info" role="alert" style="%s">\n', css)
  } else {
    cat('<div class="alert alert-info" role="alert">\n')
  }
  if (is.character(icon)) {
    printf('  <span class="glyphicon glyphicon-%s" style="font-size: 1.2em;"></span>\n', icon)
  }
  eval(expr, envir=envir)
  cat('</div>\n')
} # alert_help()
