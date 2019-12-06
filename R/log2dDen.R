##----------------------------------------------------------------------------##
## log2dDen: Calculate 2D densities on a log scale
##----------------------------------------------------------------------------##

#' @name log2dDen
#' @title Calculate 2D densities on a log scale
#' 
#' @param x Numeric input for x dimension
#' @param y Numeric input for y dimension
#' @param nbin Integer of length 1, the number of bins in each dimension
#' @param lims Numeric of length 4, ranges for the x and y values, respectively
#' @param freq Logical of length 1, counts converted to proportions when FALSE
#' 
#' @return list with x, y: seq(1, nbin) for plotting; z: nbin x nbin matrix 
#' with the freq/proportion values at the x and y bins; xseq, yseq: numeric
#' vectors containing the cut points for the x and y bins.
#' 
#' @export

log2dDen <- function(x, y, nbin = 100, lims = c(range(x), range(y)), 
                     freq = FALSE) {
  
  ## input checks
  stopifnot(l1log(freq))
  stopifnot(l1int(type.convert(nbin)))
  stopifnot(is.numeric(lims) && length(lims) == 4)
  stopifnot(is.numeric(x) && is.numeric(y))
  
  xseq <- lseq(lims[1], lims[2], length.out = nbin)
  yseq <- lseq(lims[3], lims[4], length.out = nbin)
  fdat <-  as.data.table(table(findInterval(x, xseq, all.inside = TRUE),
                               findInterval(y, yseq, all.inside = TRUE)))
  setnames(fdat, c("x", "y", "N"))
  fdat <- fdat[ , lapply(.SD, type.convert)]
  fdat <- fdat[N != 0]
  fmat <- diag(nbin)*0
  fmat[as.matrix(fdat[ , .(x, y)])] <- fdat[ , N]
  fmat[fmat == 0] <- NA
  if (!freq) fmat <- fmat/sum(fmat, na.rm = TRUE)
  d <- list(x = 1:nbin, y = 1:nbin, z = fmat, xseq = xseq, yseq = yseq)
  d
  
}