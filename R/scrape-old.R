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
        "vignettes/naive-genotyping",
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
##        "howtos/AccessSourceCode",
        "howtos/ImproveProcessingTime",
        "howtos/SetupOfAromaUnitNnnCnBinarySet",
        "howtos/bpmapCluster2Cdf", ## Create a CDF (and associated) files from a BpMap file (tiling arrays)
        "howtos/CreateUGP", ## Create a Unit Genome Position (UGP) file
        "howtos/how-access-cel-files-non-standard-directory",
        "howtos/setFullNamesTranslator",
        "howtos/exportTotalCnRatioSet",
        "howtos/exportTotalAndFracB",
        "howtos/writeDataFrame",
        "howtos/extractDataFrame",
        "howtos/extractTheta",
        "howtos/extractAffyBatch",
        "howtos/extractExpressionSet",
        "howtos/extractESet",
        "howtos/extractSnpQSet",
        "howtos/using_aroma_in_bioc",
        "howtos/using_bioc_in_aroma",
        "howtos/MigrateAndReproduceAnAnalysisElsewhere",
        "howtos/migrate_to_other_system",
        "howtos/CRMAv2_on_subset_of_arrays",
        "howtos/ImproveProcessingTime",
        "howtos/DeleteIntermediateDataFilesWhileKeepingFinalOnes",
        "howtos/ProcessCELFilesWithDifferentChipTypeAliases",
        "howtos/create_CDF_from_scratch",
        "howtos/bpmapCluster2Cdf",
        "howtos/createCdfFromBioconductorPlatformDesignInfo",
        "howtos/createCdfFromBioconductorCdfPackage",
        "howtos/CreateUGP",
        "howtos/CreateAUnitFragmentLengthFile",
        "howtos/CreateAcsFile",
        "howtos/annotationData-writeDataFrame",
        "howtos/UseLongFilenamesOnWindows",

      "troubleshooting",
        "troubleshooting/DirectoryStructures",

    "docs",
      "chipTypes",
        "chipTypes/ATH1-121501",
        "chipTypes/ax13339",
        "chipTypes/Axiom_GW_Hu_SNP",
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
        "chipTypes/DroGene-1_0-st-v1",
        "chipTypes/GenomeWideSNP_5",
        "chipTypes/GenomeWideSNP_6",
        "chipTypes/HG-Focus",
        "chipTypes/HG-U133A_2",
        "chipTypes/HG-U133A_and_HG-U133B",
        "chipTypes/HG-U133_Plus_2",
        "chipTypes/HG_U95ABCDE",
        "chipTypes/HG_U95Av2",
        "chipTypes/Hs_PromPR_v02",
        "chipTypes/HT_HG-U133A_and_HT_HG-U133B",
        "chipTypes/HT_HG-U133_Plus_PM",
        "chipTypes/HT_MG-430_PM",
        "chipTypes/HuEx-1_0-st-v2",
        "chipTypes/HuGene-1_0-st-v1",
        "chipTypes/HuGene-1_1-st-v1",
        "chipTypes/HuGeneFL",
        "chipTypes/Mapping10K_Xba131",
        "chipTypes/Mapping10K_Xba142",
        "chipTypes/Mapping250K_Nsp-and-Mapping250K_Sty",
        "chipTypes/Mapping50K_Hind-and-Xba240",
        "chipTypes/miRNA-1_0",
        "chipTypes/miRNA-2_0",
        "chipTypes/Mitochip_2",
        "chipTypes/MitoP-1r520451",
        "chipTypes/MitoP-2r520651",
        "chipTypes/Mm_PromPR_v02",
        "chipTypes/MoEx-1_0-st-v1",
        "chipTypes/MoGene-1_0-st-v1",
        "chipTypes/MoGene-1_1-st-v1",
        "chipTypes/Mouse430_2",
        "chipTypes/Mouse430A_2",
        "chipTypes/MOUSEDIVm520650",
        "chipTypes/PrimeView",
        "chipTypes/RaEx-1_0-st-v1",
        "chipTypes/RaGene-1_0-st-v1",
        "chipTypes/Rat230_2",
        "chipTypes/Test3",
        "chipTypes/X_laevis_2",

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

    "packages/aroma",

    ## Non-published
    "chipTypes/HuEx-1_0-st-v2/BrainArrayENSEandENSG",
    "projects/ChromosomeExplorer2.0",
    "replication/MPCBSandCBS"
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
      opts <- c("--atx-headers")
#      opts <- NULL
      system2("pandoc", args=c("-s", url, opts, "-o", file))
    }
  } # for (dir ...)
} # download()

githubCodeBlock <- function(bfr, ...) {
  rows <- grep("^    ", bfr)
  # Nothing to do?
  if (length(rows) == 0L) return(bfr)

  seqs <- seqToIntervals(rows)
  pre <- seqs[,1]-1L
  keep <- (nchar(trim(bfr[pre])) == 0L)
  seqs <- seqs[keep,,drop=FALSE]
  post <- seqs[,2]+1L
  keep <- (nchar(trim(bfr[post])) == 0L)
  seqs <- seqs[keep,,drop=FALSE]

  # Nothing to do?
  if (nrow(seqs) == 0L) return(bfr)

  pres <- posts <- NULL
  for (kk in seq(nrow(seqs))) {
    from <- seqs[kk,1]
    to <- seqs[kk,2]
    rows <- from:to
    bfr[rows] <- gsub("^    ", "", bfr[rows])
  #  print(sprintf("%02d. %s\n", rows, bfr[rows]))
    pres <- c(pres, from)
    posts <- c(posts, to+1L)
  }
  values <- c(rep("```r", length(pres)), rep("```", length(posts)))
  bfr <- insert(bfr, ats=c(pres, posts), values=values)

  bfr
} # githubCodeBlock()


githubCodeBlockAuto <- function(bfr, ...) {
  bfr <- c(bfr, "")
  empty <- which(nchar(trim(bfr)) == 0L)
  # Nothing to do?
  if (length(empty) <= 1L) return(bfr)

  # Drop empty lines inside GitHub code blocks?
  for (kk in seq_len(length(empty)-1L)) {
    from <- empty[kk]+1L
    if (grepl("^```", bfr[from])) {
      mprintf("Found beginning: %d\n", from)
      for (ll in (kk+1L):(length(empty)-1L)) {
        to <- empty[ll]-1L
        if (grepl("^```", bfr[to])) {
          mprintf("Found ending: %d\n", to)
          # Found end
          empty[kk:(ll-1L)] <- NA_integer_
          break
        }
      } # for (ll ...)
    } # for (kk ...)
  }
  empty <- na.omit(empty)

  pres <- posts <- NULL

  # For each potential code block...
  for (kk in seq_len(length(empty)-1L)) {
    from <- empty[kk]+1L
    to <- empty[kk+1L]-1L
    if (to < from) next

    rows <- from:to
    code <- bfr[rows]

    # (a) Drop for sure
    # A header?
    if (any(grepl("^[-]+$", code))) next
    if (any(grepl("^[=]+$", code))) next

    # A list?
    if (any(grepl("^-   ", code))) next

    # A label?
    if (grepl(":$", code[length(code)])) next

    # Already GitHub code block?
    if (grepl("^```", code[1]) || grepl("^```", code[length(code)])) {
      code <- gsub(";(|  )$", "", code)
      bfr[rows] <- code
      next
    }

    # (b) Keep for sure
    isCode <- FALSE
    if (any(grepl("^[\\]> ", code))) {
      isCode <- TRUE
    } else if (any(grepl(" [\\]<- ", code))) {
      isCode <- TRUE
    } else if (any(grepl("[\\][$]", code))) {
      isCode <- TRUE
    } else if (any(grepl("^[ ]*RAM: [0-9]+.[0-9]+([a-zA-Z]B|bytes)(|[\\]|  )$", code))) {
      isCode <- TRUE
    } else if (any(grepl(" (<|&lt;)- ", code)) && any(grepl(";(|[\\]|  )$", code))) {
      isCode <- TRUE
    } else if (length(code) == 1L) {
      if (grepl("^[ ]*library[(](|'|\")[a-zA-Z0-9.]+(|'|\")[)](|;)(|  )$", code)) {
        isCode <- TRUE
      }
    } else if (length(code) > 1L) {
      if (all(grepl("^(#|[\\]#)+ ", code))) {
        isCode <- TRUE
      }
      ## printf("%02d [%5s]: '%s'\n", rows, grepl("^(#|[\\]#)+ ", code), code)
      ## printf("\n")
    }

    if (!isCode) next

    # Unescape
    code <- gsub("^[ ]*[\\]> ", "> ", code)
    code <- gsub(" [\\]<- ", " <- ", code)
    code <- gsub(" &lt;- ", " <- ", code)
    code <- gsub("[\\][$]", "$", code)
    code <- gsub("[\\]_", "_", code)
    code <- gsub("[\\]#", "#", code)
    code <- gsub("[\\]<", "<", code)
    code <- gsub("[\\]>", ">", code)
    code <- gsub("^[ ]([^ ])", "\\1", code)
    code <- gsub("[\\]$", "", code)
    code <- gsub(";(|  )$", "", code)

    bfr[rows] <- code
    pres <- c(pres, from)
    posts <- c(posts, to+1L)

    ## printf("%02d: %s\n", rows, code); printf("\n")
  }

  if (length(pres) > 0L) {
    values <- c(rep("```r", length(pres)), rep("```", length(posts)))
    bfr <- insert(bfr, ats=c(pres, posts), values=values)
  }

  # Merge succeeding code blocks
  bfr <- paste(bfr, collapse="\n")
  bfr <- gsub("```\n\n```r\n", "\n", bfr, fixed=TRUE)
  bfr <- unlist(strsplit(bfr, split="\n", fixed=TRUE))

  bfr
} # githubCodeBlockAuto()


clean <- function() {
  # All downloaded files
  files <- list.files("md", pattern="[.]md$", recursive=TRUE)
  for (file in files) {
    fileS <- file.path("md", file)
    fileD <- file.path("md,trimmed", file)
    if (!file_test("-f", fileD) || file_test("-nt", fileS, fileD)) {
      mkdirs(dirname(fileD))
      printf("Trimming: %s -> %s\n", file, fileD)

      # Read
      bfr <- readLines(fileS, warn=FALSE)

      # Trim
      start <- grep("/user/password", bfr, fixed=TRUE) + 4L
      if (length(start) == 0L) {
        start <- grep("**Search forum:**", bfr, fixed=TRUE) + 4L
        if (length(start) == 0L) start <- 1L
      }
#      mprintf("start: %d\n", start)
      end <- grep("Copyright Henrik Bengtsson et al.", bfr, fixed=TRUE) - 1L
#      mprintf("end: %d\n", end)
      if (length(end) == 0L) end <- length(bfr)
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

      # Markdown Code blocks to GitHub-flavored code blocks
      bfr <- githubCodeBlock(bfr)

      # Automagically convert what looks like code blocks to code blocks
      bfr <- githubCodeBlockAuto(bfr)

      # White space (except for code blocks starting with 4 spaces)
#      bfr <- trim(bfr)
      bfr <- gsub("^[ \t]{5,}$", "", bfr)
      bfr <- gsub("^[ \t]{1,3}$", "", bfr)
      bfr <- gsub("[ \t]+$", "", bfr)

      # Trim odd characters
      bfr <- gsub(" ", " ", bfr)
      bfr <- gsub("Â", " ", bfr) # Hard space to soft space

      # Markdown translation
      bfr <- gsub("[\\]$", "  ", bfr)

      bfr <- gsub("[\\]_", "_", bfr)
      bfr <- gsub("[\\]^", "^", bfr)
      bfr <- gsub("[\\][*]", "*", bfr)

      # Trim whitespace in lists
#      bfr <- gsub("^[-*][ ]+", "\\1 ", bfr)

      # Write
      writeLines(bfr, con=fileD)
    }
  } # for (file ...)
} # clean()

mdToRsp <- function(fileS, fileD) {
  bfr <- readLines(fileS, warn=FALSE)
  bfr <- paste(bfr, collapse="\n")

  # Drop code snippet attributes
  bfr <- gsub("``` [{][.]brush:[^}]+[}]", "```", bfr)

  # Translate UTF-8 symbols
  bfr <- gsub('“', '"', bfr, fixed=TRUE)
  bfr <- gsub('”', '"', bfr, fixed=TRUE)
  bfr <- gsub('™', '', bfr, fixed=TRUE)

  # Dynamic download links
#  cat("\n\n1.--------------\n"); cat(bfr)
  bfr <- gsub(".gz](/data/annotationData/", "](/data/annotationData/", bfr, fixed=TRUE)
#  cat("\n\n2.--------------\n"); cat(bfr)

  # (a) Markdown links for chip-type data
  bfr <- gsub("\\[[^]]+\\][(]/data/annotationData/chipTypes/([^/)]+)(|/)([^)]*)[)]", "<%=chipTypeData('\\1', '\\3')%>", bfr)
#  cat("\n\n3.--------------\n"); cat(bfr)

  # (b) All other Markdown links to local files
  bfr <- gsub("[(](/[^)]+)[)]", "(<%=pathTo('\\1')%>)", bfr)
#  cat("\n\n4.--------------\n"); cat(bfr)

  # Sanity check
  stopifnot(!any(grepl("chipTypeData(')", bfr, fixed=TRUE)))

  # Image links
  bfr <- gsub("([.][.]/)*/sites/default/files/images/public/", "assets/images/", bfr)
  bfr <- gsub("([.][.]/)*/sites/default/files/images/u[5-8]/", "assets/images/", bfr)
  bfr <- gsub("([.][.]/)*/sites/default/files/imagecache/fast_gallery_thumb/images/public/", "assets/images_thumbs/", bfr)
  bfr <- gsub("([.][.]/)*/sites/default/files/images/", "assets/images/", bfr)
  bfr <- gsub("/images/public/", "assets/images/", bfr)

  cat(bfr, file=fileD)
} # mdToRsp()

torsp <- function() {
  # All downloaded files
  pathS <- "md,trimmed"
  files <- list.files(pathS, pattern="[.]md$", recursive=TRUE)

  pathD <- "md,trimmed,rsp"
  for (file in files) {
    path <- dirname(file)
    fileS <- file.path("md,trimmed", file)
    fileD <- file.path(pathD, path, "index.md.rsp")
    if (!file_test("-f", fileD) || file_test("-nt", fileS, fileD)) {
      mkdirs(dirname(fileD))
      printf("Copying: %s -> %s\n", fileS, fileD)
      mdToRsp(fileS, fileD)
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
    if (force || !file_test("-f", fileD) || file_test("-nt", fileS, fileD)) {
      printf("Compiling: %s -> %s\n", fileS, fileD)

      # Find page title
      bfr <- readLines(fileS, warn=FALSE)
      idx <- which(nzchar(bfr))[1L]
      page <- trim(bfr[idx])

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

      options(markdown.HTML.options="fragment_only")
      args <- list()
      args$pathToRoot <- pathToRoot
      args$chipTypeData <- chipTypeData
      args$pathTo <- pathTo
      md <- rfile(fileS, args=args, workdir=pathD, postprocess=FALSE)
      html <- RspFileProduct(gsub("[.]md$", ".html", md), mustExist=FALSE)
      markdownToHTML(md, html, encoding="UTF-8")
      print(html)
    }
  } # for (file ...)
} # tohtml()

download(); clean(); torsp();
