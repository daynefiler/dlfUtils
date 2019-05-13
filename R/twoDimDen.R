#' @name twoDimDen
#' @title Create 2d density on log-scale
#' 
#' @param x the x values
#' @param y the y values
#' @param nbin the number of bins for the density calculation
#' @param lims vector containing the x and y limits c(x1, x2, y1, y2)
#' @param freq logical, return frequency? density is returned if FALSE
#' @inheritParams graphics::plot.window
#' 
#' @return List with: 'x' and 'y' giving the bin number for x and y; 'z' matrix
#' giving the frequency/density in each bin; 'xseq' and 'yseq' giving the break
#' points for the x and y bins 
#' 
#' @import data.table
#' @export


twoDimDen <- function(x, y, nbin = 100, lims = NULL, 
                      freq = FALSE, log = FALSE) {
  if (is.null(lims)) {
    lims <- c(min(x), max(x), min(y), max(y))
  }
  if (log) {
    xseq <- lseq(lims[1], lims[2], length.out = nbin)
    yseq <- lseq(lims[3], lims[4], length.out = nbin)
  } else {
    xseq <- seq(lims[1], lims[2], length.out = nbin)
    yseq <- seq(lims[3], lims[4], length.out = nbin)
  }
  
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
}