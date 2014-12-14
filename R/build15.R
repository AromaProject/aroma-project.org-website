library("R.utils")

tohtml <- function(force=FALSE) {
  use("R.rsp")
  use("markdown")

  # All downloaded files
  pathS <- "md,trimmed,rsp"
  files <- list.files(pathS, pattern="[.]rsp$", recursive=TRUE)

  for (file in files) {
    path <- dirname(file)
    fileS <- file.path(pathS, file)
    pathD <- file.path("html", path)
    fileD <- file.path(pathD, gsub(".md.rsp", ".html", basename(fileS)))
    if (force || !file_test("-f", fileD) || file_test("-nt", fileS, fileD)) {
      printf("Compiling: %s -> %s\n", fileS, fileD)

      # Find page title
      body <- readLines(fileS, warn=FALSE)
      idx <- which(nzchar(body))[1L]
      page <- trim(body[idx])

      # Find depth
      if (path == ".") {
        pathToRoot <- "."
      } else {
        depth <- length(unlist(strsplit(path, split="/")))
        pathToRoot <- paste(c(rep("..", times=depth), ""), collapse="/")
      }

      pathTo <- function(pathname) {
        url <- sprintf("%s/%s", pathToRoot, pathname)
        if (!grepl("[.](html|pdf|png|gif|css|js|ico)$", pathname, ignore.case=TRUE)) {
          url <- sprintf("%s/index.html", url)
        }
        url <- gsub("[/]+", "/", url)
        url
      } # pathTo()

      chipTypeData <- function(chipType, filename) {
        url <- sprintf("http://aroma-project.org/data/annotationData/chipTypes/%s/%s", chipType, filename)
        sprintf("[%s](%s)", filename, url)
      } # chipTypeData()


      # Compile RSP Markdown to Markdown
      mcat("RSP Markdown -> Markdown...\n")
      args <- list()
      args$pathToRoot <- pathToRoot
      args$chipTypeData <- chipTypeData
      args$pathTo <- pathTo
      body <- rstring(body, type="application/x-rsp", args=args, workdir=pathD)
      mcat("RSP Markdown -> Markdown...done\n")

      # Compile Markdown to HTML
      mcat("Markdown -> HTML...\n")
      body <- markdownToHTML(text=body, options="fragment_only", encoding="UTF-8")
      mcat("Markdown -> HTML...done\n")

      # Compile RSP HTML with content
      mcat("HTML + template -> HTML...\n")
      args$body <- body
      mcat("RSP arguments:\n")
      mstr(args)
      html <- rfile("templates/index.html.rsp", args=args, workdir=pathD)
      mcat("HTML + template -> HTML...done\n")

      print(html)
    }
  } # for (file ...)
} # tohtml()

if (!file_test("-d", "html/assets")) {
  copyDirectory("assets", "html/assets", recursive=TRUE, skip=TRUE)
}

tohtml()

