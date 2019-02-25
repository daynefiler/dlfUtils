#-------------------------------------------------------------------------------
# lseq: log sequence
#-------------------------------------------------------------------------------

#' @name lseq
#' @title Create a log-spaced sequence
#' 
#' @inheritParams base::seq
#' 
#' @importFrom graphics text par
#' @export


lseq <- function(from, to, length.out, by) {
  if (missing(length.out) && missing(by)) {
    stop("Must provide length.out or by.")
  }
  if (!missing(length.out) && !missing(by)) {
    stop("Cannot provide both length.out and by.")
  }
  if (!missing(length.out)) {
    return(exp(seq(log(from), log(to), length.out = length.out)))
  } else {
    return(exp(seq(log(from), log(to), by = by)))
  }
  
}