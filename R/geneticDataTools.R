
getNcbiFeatureTable <- function(v = "GCF_000001405.30_GRCh38.p4") {
  ## v: chr of length 1, the NCBI version 
  if(!curl::has_internet()) stop("Must be connected to the internet.")
  d <- file.path("ftp:/", 
                 "ftp.ncbi.nih.gov", 
                 "genomes", 
                 "refseq", 
                 "vertebrate_mammalian", 
                 "Homo_sapiens", 
                 "all_assembly_versions",
                 v)
  f <- sprintf("%s_feature_table.txt.gz", v)
  # thiscall <- match.call()
  # tbl <- tryCatch({
  #   a <- fread(file.path(d, f), showProgress = FALSE)
  # }, error = function(w) {
  #   w$call <- thiscall
  #   w$message <- "Could not download the feature file for the given version."
  #   stop(w)
  # })
  tbl <- try(suppressWarnings(fread(file.path(d, f), showProgress = FALSE)), 
             silent = TRUE)
  if (is(tbl, "try-error")) {
    m <- "Could not download the feature file for the given version."
    c <- match.call()
    stop(simpleError(m, c))
  }
  setnames(tbl, "# feature", "feature")
  tbl[]
}

getUcscCytoBands <- function(db = "hg38", oneBased = TRUE) {
  if(!curl::has_internet()) stop("Must be connected to the internet.")
  ucsc <- dbConnect(MySQL(), 
                    username = "genome", 
                    dbname = db,
                    host = "genome-mysql.soe.ucsc.edu",
                    port = 3306,
                    password = "")
  dat <- suppressWarnings(dbGetQuery(ucsc, "SELECT * FROM cytoBand;"))
  dat <- as.data.table(dat)
  dbDisconnect(ucsc)
  if (oneBased) {
    dat[, chromStart := chromStart + 1]
  }
  dat[]
}

getUcscQuery <- function(qstr, db = "hg38") {
  if(!curl::has_internet()) stop("Must be connected to the internet.")
  ucsc <- dbConnect(MySQL(), 
                    username = "genome", 
                    dbname = db,
                    host = "genome-mysql.soe.ucsc.edu",
                    port = 3306,
                    password = "")
  dat <- suppressWarnings(dbGetQuery(ucsc, qstr))
  dat <- as.data.table(dat)
  dbDisconnect(ucsc)
  warning("UCSC uses 0-based start and 1-based end positions.", call. = FALSE)
  dat[]
}

getNcbiChr2Acc <- function(v = "GCF_000001405.26_GRCh38") {
  ## v: chr of length 1, the NCBI version 
  if(!curl::has_internet()) stop("Must be connected to the internet.")
  d <- file.path("ftp:/", 
                 "ftp.ncbi.nih.gov", 
                 "genomes", 
                 "refseq", 
                 "vertebrate_mammalian", 
                 "Homo_sapiens", 
                 "all_assembly_versions",
                 v, 
                 sprintf("%s_assembly_structure", v))
  f <- file.path("assembled_chromosomes", "chr2acc")
  m1 <- try(silent = TRUE, {
    suppressWarnings(fread(file.path(d, "Primary_Assembly", f), 
                           showProgress = FALSE))
  })
  if (is(m1, "try-error")) {
    m <- "Could not download the chr2acc file for the given version."
    c <- match.call()
    stop(simpleError(m, c))
  }
  m1[ , prefix := "g"]
  m2 <- try(silent = TRUE, {
    suppressWarnings(fread(file.path(d, "non-nuclear", f), 
                           showProgress = FALSE))
  })
  if (is(m2, "try-error")) {
    m <- "Could not download the chr2acc file for the given version."
    c <- match.call()
    stop(simpleError(m, c))
  }
  m2[ , prefix := "m"]
  out <- rbind(m1, m2)
  setnames(out, c("chr", "acc", "prefix"))
  out[]
}

