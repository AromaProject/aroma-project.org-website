R.utils::use("R.utils")

download <- function() {
  root <- "http://www.aroma-project.org"
  dirs=c(
    ".",
    "getstarted",
      "install",

      "setup",
        "setup/annotationData",
        "setup/LocationOfRawDataFiles",
        "setup/QuickSummaryOfRequiredFileStructure",

      "vignettes",
        # Copy-number preprocessing
        "vignettes/CRMAv2",
        "vignettes/CRMAv1",
        "vignettes/ACNE",
        "vignettes/CalMaTe",
        "vignettes/tumorboost-highlevel",
        "vignettes/tumorboost-lowlevel",
        "vignettes/pairedTotalCopyNumberAnalysis",
        "vignettes/Sex-chromosome_bias-corrected_reference_signals",
        "vignettes/calculating_raw_total_copy_numbers_manually",
        # Copy-number segmentation
        "vignettes/NonPairedCBS",
        "vignettes/PairedPSCBS-lowlevel",
        # Integrating copy numbers from multiple platforms
        "vignettes/MSCN",
        "vignettes/CreatingBinaryDataFilesContainingCopyNumberEstimates",
        # Genotyping
        "vignettes/CRLMM100K500K",
        # Deprecated
        "vignettes/total_copy_number_analysis",
        "vignettes/paired_total_copy_number_analysis",
        # RNA expression analysis
        "vignettes/GeneSTArrayAnalysis",
        "vignettes/FIRMA-HumanExonArrayAnalysis",
        "vignettes/UsingGenomeGraphsWithFIRMA",
        # Tiling array analysis
        "vignettes/MAT-TilingArrayAnalysis",
        # Miscellaneous
        "vignettes/probe-signal_densities_and_rank-based_quantile_normalization",

      "blocks",
        "blocks/doCRMAv1",
        "blocks/doCRMAv2",
        "blocks/doRMA",

      "howtos",
        "howtos/ImproveProcessingTime",
        "howtos/UseLongFilenamesOnWindows",
        "howtos/SetupOfAromaUnitNnnCnBinarySet",
        "howtos/bpmapCluster2Cdf", ## Create a CDF (and associated) files from a BpMap file (tiling arrays)
        "howtos/CreateUGP", ## Create a Unit Genome Position (UGP) file

      "troubleshooting",
        "troubleshooting/DirectoryStructures",

    "docs",
      "chipTypes",
        "chipTypes/ATH1-121501",
        "chipTypes/Canine_2",
        "chipTypes/CentHindAv2_and_CentXbaAv2",
        "chipTypes/Citrus",
        "chipTypes/Citrus_SNP",
        "chipTypes/CytoScan750K_Array",
        "chipTypes/CytoScanHD_Array",
        "chipTypes/Cytogenetics_Array",
        "chipTypes/DMET_Plus",
        "chipTypes/Dm_tiling2_MR_v01",
        "chipTypes/DogSty06m520431",
        "chipTypes/GenomeWideSNP_5",
        "chipTypes/GenomeWideSNP_6",
        "chipTypes/HG-Focus",
        "chipTypes/HG-U133A_2",
        "chipTypes/HG-U133A_and_HG-U133B",
        "chipTypes/HG-U133_Plus_2",
        "chipTypes/HG_U95ABCDE",
        "chipTypes/HG_U95Av2",
        "chipTypes/HT_HG-U133A_and_HT_HG-U133B",
        "chipTypes/HT_HG-U133_Plus_PM",
        "chipTypes/HT_MG-430_PM",
        "chipTypes/Hs_PromPR_v02",
        "chipTypes/HuEx-1_0-st-v2 (and HuEx-1_0-st-v1",
        "chipTypes/HuGene-1_0-st-v1",
        "chipTypes/HuGene-1_1-st-v1",
        "chipTypes/HuGeneFL (a.k.a. Hu6800",
        "chipTypes/MOUSEDIVm520650",
        "chipTypes/Mapping10K_Xba131",
        "chipTypes/Mapping10K_Xba142",
        "chipTypes/Mapping250K_Nsp-and-Mapping250K_Sty",
        "chipTypes/Mapping50K_Hind-and-Xba240",
        "chipTypes/Mitochip_2",
        "chipTypes/Mm_PromPR_v02",
        "chipTypes/MoEx-1_0-st-v1",
        "chipTypes/MoGene-1_0-st-v1",
        "chipTypes/MoGene-1_1-st-v1",
        "chipTypes/Mouse430_2",
        "chipTypes/PrimeView",
        "chipTypes/RaEx-1_0-st-v1",
        "chipTypes/RaGene-1-0-st-v1",
        "chipTypes/Rat230_2",
        "chipTypes/Test3",
        "chipTypes/X_laevis_2",
        "chipTypes/ax13339",
        "chipTypes/miRNA-1_0",
        "chipTypes/miRNA-2_0",

      "definitions",
        "definitions/namesAndTags",
        "definitions/filenames_fullnames_and_filename_extensions",
        "definitions/chipTypesAndCDFs",
      "settings",
        "docs/HowDataFilesAndDataSetsAreLocated",
      "dosAndDonts",

    "features",
      "screenshots",
      "demos",
      "replication",
        "replication/RMA",
        "replication/gcRMA",
        "replication/RmaPlm_and_affyPLM",
        "replication/SNPRMA",
        "replication/FIRMA_using_RLM_and_median_polish",

      "limitations",

      "features/benchmarks",
        "features/benchmarks/ACNE-Complexity",
        "benchmarks/DNAcopy_v1.19.2-speedup",
      "features/future",

    "resources",
      "publications",
      "presentations",
      "labsessions",
      "addons",
      "datasets",
        "datasets/Affymetrix",
        "datasets/Illumina",
      "resources/external",
      "external/ToolsForAffymetrix",

    "forum",
    "FAQ",
    "developers",
    "about",

    "getinvolved",

    "packages/aroma"
  )

  for (dir in dirs) {
    path <- file.path("md", dir)
    str(path)
    mkdirs(path)
    prefix <- file.path(path, "index")
    file <- sprintf("%s.md", prefix)
    if (!file_test("-f", file)) {
      url <- file.path(root, dir)
      printf("Downloading: %s -> %s\n", url, file)
      system2("pandoc", args=c("-s", url, "-o", file))
    }
  } # for (dir ...)
} # download()

clean <- function() {
  # All downloaded files
  files <- list.files("md", pattern="[.]md$", recursive=TRUE)

  for (file in files) {
    fileD <- file.path("md,trimmed", file)
    if (!file_test("-f", fileD)) {
      mkdirs(dirname(fileD))
      printf("Trimming: %s -> %s\n", file, fileD)

      # Read
      bfr <- readLines(file.path("md", file), warn=FALSE)

      # Trim
      start <- grep("/user/password", bfr, fixed=TRUE) + 4L
      mprintf("start: %d\n", start)
      end <- grep("Copyright Henrik Bengtsson et al.", bfr, fixed=TRUE) - 1L
      mprintf("end: %d\n", end)
      bfr <- bfr[start:end]

      # Trim empty lines at the top
      idx <- which(nzchar(bfr))[1L]
      bfr <- bfr[idx:length(bfr)]

      # Trim empty lines at the bottom
      bfr <- rev(bfr)
      idx <- which(nzchar(bfr))[1L]
      bfr <- bfr[idx:length(bfr)]
      bfr <- rev(bfr)

      # Trim HTML/CSS markup
      bfr <- gsub("[{]style=[^}]*[}]", "", bfr)
      bfr <- trim(bfr)

      # Trim odd characters
      bfr <- gsub(" ", "", bfr)
      bfr <- gsub("Â", " ", bfr) # Hard space to soft space

      # Markdown translation
      bfr <- gsub("[\\]$", "  ", bfr)

      # Write
      writeLines(bfr, con=fileD)
    }
  } # for (file ...)
} # clean()

torsp <- function() {
  # All downloaded files
  pathS <- "md,trimmed"
  files <- list.files(pathS, pattern="[.]md$", recursive=TRUE)

  pathD <- "md,trimmed,rsp"
  for (file in files) {
    path <- dirname(file)
    fileD <- file.path(pathD, path, "index.md.rsp")
    if (!file_test("-f", fileD)) {
      mkdirs(dirname(fileD))
      fileS <- file.path("md,trimmed", file)
      printf("Copying: %s -> %s\n", fileS, fileD)
      file.copy(fileS, fileD)
    }
  } # for (file ...)
} # torsp()

tohtml <- function(force=FALSE) {
  use("R.rsp")

  # All downloaded files
  pathS <- "md,trimmed,rsp"
  files <- list.files(pathS, pattern="[.]rsp$", recursive=TRUE)

  for (file in files) {
    path <- dirname(file)
    fileS <- file.path(pathS, file)
    pathD <- file.path("md,trimmed,html", path)
    fileD <- file.path(pathD, gsub(".md.rsp", ".html", basename(fileS)))
    if (force || !file_test("-f", fileD)) {
      printf("Compiling: %s -> %s\n", fileS, fileD)

      # Find page title
      bfr <- readLines(fileS)
      idx <- which(nzchar(bfr))[1L]
      page <- trim(bfr[idx])

      # Find depth
      if (path == ".") {
        pathToRoot <- ""
      } else {
        depth <- length(unlist(strsplit(path, split="/")))
        pathToRoot <- paste(c(rep("..", times=depth), ""), collapse="/")
      }

      options(markdown.HTML.options="fragment_only")
      args <- list()
      args$pathToRoot <- pathToRoot
      html <- rfile(fileS, args=args, workdir=pathD)
      print(html)
    }
  } # for (file ...)
} # tohtml()


build <- function(force=FALSE) {
  use("R.rsp")

  # All downloaded files
  pathS <- "md,trimmed,html"
  files <- list.files(pathS, pattern="[.]html$", recursive=TRUE)

  for (file in files) {
    path <- dirname(file)
    fileS <- file.path(pathS, file)
    pathD <- file.path("html", path)
    fileD <- file.path(pathD, gsub(".md.rsp", ".html", basename(fileS)))
    if (force || !file_test("-f", fileD)) {
      printf("Compiling: %s -> %s\n", fileS, fileD)

      # Find page title
      bfr <- readLines(fileS)
      bfr <- grep("<h2>", bfr, value=TRUE)[1L]
      bfr <- gsub("(<h2>|</h2>)", "", bfr)
      page <- trim(bfr)
      if (is.na(page)) page <- ""

      # Find depth
      if (path == ".") {
        pathToRoot <- ""
      } else {
        depth <- length(unlist(strsplit(path, split="/")))
        pathToRoot <- paste(c(rep("..", times=depth), ""), collapse="/")
      }

      options(markdown.HTML.options="fragment_only")
      args <- list()
      args$pathToRoot <- pathToRoot
      args$body <- file.path("..", fileS)
      args$page <- page
      html <- rfile("includes/index.html.rsp", args=args, workdir=pathD)
      print(html)
    }
  } # for (file ...)
} # build()

download(); clean(); torsp(); tohtml();
#build()
