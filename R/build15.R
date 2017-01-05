R.utils::use("R.utils")

options(warn = 1L)
options(encoding = "iso-8859-1")

## WORKAROUND:
## With 'en_US.UTF-8' (default on Ubuntu 16.04) we get invalid
## UTF-8 chararacter string.
Sys.setlocale("LC_CTYPE", "C")

sourceDirectory("templates/R/")
#source("templates/R/aliases.R")

assert_no_plain_UTF8 <- function(x) {
  name <- substitute(x)
  x <- unlist(strsplit(x, split = "\n", fixed = TRUE))
  if (anyNA(x)) {
    bad <- rep("   ", times = length(x))
    bad[is.na(x)] <- "==>"
    lines <- sprintf("%s%3d: %s", bad, seq_along(x), sQuote(x))
    msg <- sprintf("ENCODING ERROR: Detected NA character strings in %s:\n%s\n", sQuote(name), paste(lines, collapse = "\n"))
    throw(msg)
  }
  
  if (any(grepl("<U+", x, fixed = TRUE))) {
    bad <- rep("   ", times = length(x))
    bad[grep("<U+", x)] <- "==>"
    lines <- sprintf("%s%3d: %s", bad, seq_along(x), sQuote(x))
    msg <- sprintf("ENCODING ERROR: Detected non-encoded UTF-8 character as plain text (e.g. '<U+00E9>') in %s:\n%s\n", sQuote(name), paste(lines, collapse = "\n"))
    throw(msg)
  }

  if (any(grepl("<[0-9a-f]{2}>", x, fixed = FALSE))) {
    bad <- rep("   ", times = length(x))
    bad[grep("<[0-9a-f]{2}>", x)] <- "==>"
    lines <- sprintf("%s%3d: %s", bad, seq_along(x), sQuote(x))
    msg <- sprintf("ENCODING ERROR: Detected non-encoded non-ASCII character as plain text (e.g. '<c3>') in %s:\n%s\n", sQuote(name), paste(lines, collapse = "\n"))
    throw(msg)
  }
}


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Parse options
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
input <- ecget(input="content", inherits=FALSE)
pattern <- ecget(pattern=NULL, inherits=FALSE)
force <- ecget(force=FALSE, inherits=FALSE)
verbose <- ecget(verbose=FALSE, inherits=FALSE)
verbose <- -100
mstr(list(args=list(input=input, pattern=pattern, force=force)))

tohtml <- function(path=".", root=c("scraped/5.rsp", "content,tmp", "content"), dest="html", encoding=getOption("encoding"), force=FALSE, verbose=FALSE) {
  use("R.rsp")
  use("markdown")

  # Argument 'pathS':
  root <- match.arg(root)

  # Argument 'dest':
  mkdirs(dest)


  # All downloaded files
  pathS <- file.path(root, path)
  files <- list.files(pathS, pattern="[.]rsp$", recursive=TRUE)

  # Subset of files?
  if (!is.null(pattern)) {
    files <- grep(pattern, files, value=TRUE)
  }

  for (file in files) {
    fileS <- file.path(pathS, file)
    dir <- dirname(file)
    pathD <- file.path(dest, dir)
    fileD <- file.path(pathD, gsub(".md.rsp", ".html", basename(fileS)))

##    mstr(list(fileS=file.info(fileS), fileD=file.info(fileD)))

    if (force || !file_test("-f", fileD) || file_test("-nt", fileS, fileD)) {
      mprintf("Compiling: %s -> %s\n", fileS, fileD)

      # Read content
      mcat("Read raw body ...\n")
      body_raw <- readLines2(fileS, warn=FALSE, encoding = encoding)
      assert_no_plain_UTF8(body_raw)
      
      # Convert any non-ASCII strings into UTF-8 strings
      body_raw <- iconv(body_raw, from = encoding, to="UTF-8")
      mprintf("Encoding of raw body: %s\n", hpaste(unique(sQuote(Encoding(body_raw)))))
      stopifnot(all(Encoding(body_raw) %in% c("unknown", "UTF-8")))

      # Find depth
      if (dir == ".") {
        pathToRoot <- "."
      } else {
        depth <- length(unlist(strsplit(dir, split="/")))
        pathToRoot <- paste(c(rep("..", times=depth), ""), collapse="/")
      }

      pathTo <- function(pathname) {
        if (pathToRoot == ".") {
          url <- pathname
        } else {
          url <- sprintf("%s/%s", pathToRoot, pathname)
        }
        if (!grepl("[.](html|pdf|png|gif|css|js|ico|pl|ppt|pptx|R|saf)$", pathname, ignore.case=TRUE)) {
          url <- sprintf("%s/index.html", url)
        }
        url <- gsub("[/]+", "/", url)
        url
      } # pathTo()

      # Find page title
      if (pathToRoot == ".") {
        page <- ""
      } else {
        idx <- which(nzchar(body_raw))[1L]
        page <- trim(gsub("^[ ]*[#]+ *", "", body_raw[idx]))
        mstr(page)
      }

      assign("body_raw", body_raw, envir = globalenv())

      assert_no_plain_UTF8(body_raw)

      # Compile RSP Markdown to Markdown
      mcat("RSP Markdown -> Markdown...\n")
      args <- list()
      args$pathToRoot <- pathToRoot
      args$chipTypeData <- chipTypeData
      args$pathTo <- pathTo
      args$page <- page
      body_md <- local({
        oopts <- options(encoding = "UTF-8")
	on.exit(options(oopts))
        rstring(body_raw, type="application/x-rsp", args=args, workdir=pathD, encoding = "UTF-8")
      })
      mprintf("Encoding of RSP processed body: %s\n", hpaste(unique(sQuote(Encoding(body_md)))))
      stopifnot(all(Encoding(body_md) %in% c("unknown", "UTF-8")))
      assign("body_md0", body_md, envir = globalenv())

      ## WORKAROUND: R.rsp outputs UTF-8 characters as "<U+NNNN>" strings,
      ## whereas ideally they should be outputted as '\uNNNN' characters.
      ## As a workaround, we here translate those into HTML-escaped
      ## characters, i.e. "<U+NNNN>" becomes "&#xNNNN;".
      body_md <- gsub("<U[+]([0-9A-F]{4})>", "&#x\\1;", body_md)
      assign("body_md", body_md, envir = globalenv())
      assert_no_plain_UTF8(body_md)

      mcat("RSP Markdown -> Markdown...done\n")

      # Compile Markdown to HTML
      mcat("Markdown -> HTML...\n")
      body_html <- markdownToHTML(text=body_md, options="fragment_only", encoding = "UTF-8")
      mprintf("Encoding of Markdown processed body: %s\n", hpaste(unique(sQuote(Encoding(body_html)))))
      assert_no_plain_UTF8(body_html)
      stopifnot(all(is.element(Encoding(body_html), c("unknown", "UTF-8"))))
      mcat("Markdown -> HTML...done\n")

      # Compile RSP HTML with content
      mcat("HTML + template -> HTML...\n")
      args$body <- body_html
      args$charset <- "UTF-8" ## Forced by markdown::markdownToHtml()
      args$editURL <- file.path("https://github.com/AromaProject/aroma.project.org-website/tree/master/content", file, fsep="/")
      args$url.path <- sprintf("%s/", gsub("html/", "", pathD))
      mcat("RSP arguments:\n")
      mstr(args)
      mprint(args$body)
      assign("body", args$body, envir = globalenv())
      html <- local({
        oopts <- options(encoding = "UTF-8")
        on.exit(options(oopts))
        rfile(file="templates/index.html.rsp", args=args, workdir=pathD, encoding = "UTF-8", verbose=verbose)
      })

      mcat("HTML + template -> HTML...done\n")

#      mprint(html)
    }
  } # for (file ...)
} # tohtml()

if (!file_test("-d", "html/assets")) {
  copyDirectory("assets", "html/assets", recursive=TRUE, skip=TRUE)
}

if (input == "content,tmp") {
  root <- "content,tmp"
  tohtml(root=root, force=force, verbose=verbose)
} else if (input == "content") {
  root <- "content"
  tohtml(root=root, force=force, verbose=verbose)
} else if (input == "scrape") {
  root <-"scraped/5.rsp"
  tohtml(root=root, dest="content,scraped", force=force, verbose=verbose)
  str(root)
} else {
  throw("Unknown '--input' value: ", input)
}

