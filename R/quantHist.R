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
  u <- x <= quantile(x, uq, na.rm = TRUE)
  l <- x >= quantile(x, lq, na.rm = TRUE)
  hist(x[u & l], ...)
}