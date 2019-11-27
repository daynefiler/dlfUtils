#' @name slurmRsync
#' @title Create slurm job array for rsync of multiple files
#' @description Create slurm job array for rsync of multiple files
#' @param spath character, the source to be copied 
#' @param dpath character of length 1, the destination location
#' @param rel logical of length 1, should the rsync be done relatively (using
#' the -R or --relative option)?
#' @param relTo when rel = TRUE, the path to copy relative to
#' @details 
#' Submits a slurm array for rsync of multiple files.
#' @importFrom R.utils fileAccess isDirectory
#' @export

slurmRsync <- function(spath, dpath, rel = TRUE, relTo = dirname(spath),
                       rsyncOpts = "avP") {
  
  stopifnot(l1chr(dpath))
  stopifnot(l1log(rel))
  stopifnot(is.null(relTo) || l1chr(relTo))
  if (Sys.which("sbatch") == "") stop("Only applicable on slurm clusters.")
  if (Sys.which("rsync") == "") stop("rsync not found on the system path.")
  if (fileAccess(spath, mode = 0) != 0) stop("Given source does not exist.")
  if (fileAccess(spath, mode = 1) != 0) stop("Cannot read given source.")
  if (isDirectory(spath)) {
    spath <- list.files(spath, recursive = TRUE, full.names = TRUE)
  }
  if (rel && !is.null(relTo)) {
    relTo <- basename(relTo)
    pathParts <- strsplit(spath, .Platform$file.sep)
    if (!all(grepl(relTo, pathParts))) stop("'relTo' does not exist in spath.")
    addPeriod <- function(x) {
      Reduce(file.path, append(x, ".", grep(relTo, x)[1]))
    }
    spath <- sapply(pathParts, addPeriod)
  }
  rsync <- function(spath, dpath, opt) {
    system(sprintf("rsync -%s %s %s", opt, spath, dpath))
  }
  p <- data.frame(spath = spath, 
                  dpath = dpath, 
                  opt = paste0(rsyncOpts, ifelse(rel, "R", "")), 
                  stringsAsFactors = FALSE)
  slurmArray(rsync, p, "dlfRsync")
  
}