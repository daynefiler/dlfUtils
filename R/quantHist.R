##----------------------------------------------------------------------------##
## quantHist: Draw histogram of reduced range, by lower and upper quantile
##----------------------------------------------------------------------------##

#' @name quantHist
#' @title Draw histogram of reduced range, by lower and upper quantile
#' 
#' @param x Numeric input to hist
#' @param lq Lower quantile used to subset x
#' @param uq Upper quantile used to subset x
#' 
#' @export


quantHist <- function(x, lq = 0.05, uq = 0.95, ...) {
  hist(x[x <= quantile(x, uq) & x >= quantile(x, lq)], ...)
}