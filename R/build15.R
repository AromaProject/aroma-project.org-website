library("R.utils")

tohtml <- function(path=".", root=c("md,trimmed,rsp", "content"), force=FALSE) {
  use("R.rsp")
  use("markdown")

  # Argument 'pathS':
  root <- match.arg(root)

  charset <- "UTF-8"
  if (root == "content") charset <- "ISO-8859-1"

  # All downloaded files
  pathS <- file.path(root, path)
  files <- list.files(pathS, pattern="[.]rsp$", recursive=TRUE)

  for (file in files) {
    fileS <- file.path(pathS, file)
    pathD <- file.path("html", path)
    fileD <- file.path(pathD, gsub(".md.rsp", ".html", basename(fileS)))
    if (force || !file_test("-f", fileD) || file_test("-nt", fileS, fileD)) {
      mprintf("Compiling: %s -> %s\n", fileS, fileD)

      # Read content
      body <- readLines(fileS, warn=FALSE)

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

      # Find page title
      if (pathToRoot == ".") {
        page <- ""
      } else {
        idx <- which(nzchar(body))[1L]
        page <- trim(gsub("^[ ]*[#]+ *", "", body[idx]))
mstr(page)
      }


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
      args$page <- page
      body <- rstring(body, type="application/x-rsp", args=args, workdir=pathD)
      mcat("RSP Markdown -> Markdown...done\n")

      # Compile Markdown to HTML
      mcat("Markdown -> HTML...\n")
      body <- markdownToHTML(text=body, options="fragment_only", encoding="UTF-8")
      mcat("Markdown -> HTML...done\n")

      # Compile RSP HTML with content
      mcat("HTML + template -> HTML...\n")
      args$body <- body
      args$charset <- charset
      mcat("RSP arguments:\n")
      mstr(args)
      html <- rfile("templates/index.html.rsp", args=args, workdir=pathD)
      mcat("HTML + template -> HTML...done\n")

      mprint(html)
    }
  } # for (file ...)
} # tohtml()

if (!file_test("-d", "html/assets")) {
  copyDirectory("assets", "html/assets", recursive=TRUE, skip=TRUE)
}

tohtml(root="md,trimmed,rsp")
tohtml(root="content")
