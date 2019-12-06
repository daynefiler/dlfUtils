##----------------------------------------------------------------------------##
## reduce3mad: Reduce given data to med +/- 3mad
##----------------------------------------------------------------------------##

#' @name reduce3mad
#' @title Reduce given data to med +/- 3mad
#' 
#' @param x Numeric
#' @param returnBound Logical of length 1, return bounds rather than reduced set
#' 
#' @importFrom stats median mad
#' @export


reduce3mad <- function(x, returnBound = FALSE) {
  xmed <- median(x)
  xmad <- 3*mad(x)
  rmX <- x > xmed + xmad | x < xmed - xmad
  if (any(rmX)) x <- reduce3mad(x[!rmX])
  if (returnBound) return(median(x) + c(-1, 1)*mad(x))
  x
}