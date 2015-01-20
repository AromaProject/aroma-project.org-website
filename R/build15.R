R.utils::use("R.utils")

input <- cmdArg(input="content")
force <- cmdArg(force=FALSE)
mstr(list(args=list(input=input, force=force)))

tohtml <- function(path=".", root=c("scraped/5.rsp", "content,tmp", "content"), dest="html", force=FALSE) {
  use("R.rsp")
  use("markdown")

  # Argument 'pathS':
  root <- match.arg(root)

  # Argument 'dest':
  mkdirs(dest)


  charset <- "UTF-8"
##  if (grepl("content", root)) charset <- "ISO-8859-1"

  ## FIXME: R.rsp should take care of this via an argument 'encoding'
  olocale <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", olocale))
  Sys.setlocale("LC_CTYPE", "C")

  # All downloaded files
  pathS <- file.path(root, path)
  files <- list.files(pathS, pattern="[.]rsp$", recursive=TRUE)

  sourceDirectory("templates/R/", verbose=-100)

  for (file in files) {
    fileS <- file.path(pathS, file)
    dir <- dirname(file)
    pathD <- file.path(dest, dir)
    fileD <- file.path(pathD, gsub(".md.rsp", ".html", basename(fileS)))

##    mstr(list(fileS=file.info(fileS), fileD=file.info(fileD)))

    if (force || !file_test("-f", fileD) || file_test("-nt", fileS, fileD)) {
      mprintf("Compiling: %s -> %s\n", fileS, fileD)

      # Read content
      body <- local({
        con <- file(fileS, open="r", encoding="native.enc")
        on.exit(close(con))
        readLines(con, warn=FALSE)
      })

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
        idx <- which(nzchar(body))[1L]
        page <- trim(gsub("^[ ]*[#]+ *", "", body[idx]))
        mstr(page)
      }

      # Compile RSP Markdown to Markdown
      mcat("RSP Markdown -> Markdown...\n")
      args <- list()
      args$pathToRoot <- pathToRoot
      args$chipTypeData <- chipTypeData
      args$pathTo <- pathTo
      args$page <- page
      body <- rstring(body, type="application/x-rsp", args=args, workdir=pathD)
      mcat("RSP Markdown -> Markdown...done\n")

      # Compile Markdown to HTML
      mcat("Markdown -> HTML...\n")
      body <- markdownToHTML(text=body, options="fragment_only", encoding="UTF-8")
      body <- enc2utf8(body)
      mcat("Markdown -> HTML...done\n")

      # Compile RSP HTML with content
      mcat("HTML + template -> HTML...\n")
      args$body <- body
      args$charset <- charset
      args$editURL <- file.path("https://github.com/AromaProject/aroma.project.org-website/tree/master/content", file, fsep="/")
      args$url.path <- sprintf("%s/", gsub("html/", "", pathD))
      mcat("RSP arguments:\n")
      mstr(args)

      html <- local({
        if (charset == "UTF-8") {
          oopts <- options(encoding="UTF-8")
          on.exit(options(oopts))
        }
        rfile(file="templates/index.html.rsp", args=args, workdir=pathD)
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
  tohtml(root=root, force=force)
} else if (input == "content") {
  root <- "content"
  tohtml(root=root, force=force)
} else if (input == "scrape") {
  root <-"scraped/5.rsp"
  tohtml(root=root, dest="content,scraped", force=force)
  str(root)
} else {
  throw("Unknown '--input' value: ", input)
}
