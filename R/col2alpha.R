##----------------------------------------------------------------------------##
## col2alpha: convert colors to alpha form
##----------------------------------------------------------------------------##

#' @name col2alpha
#' @title Modify the alpha value for given color vector
#' 
#' @param col vector of colors as defined by any of the three R specifications 
#' @param alpha numeric of length 1, between 0 and 1
#' 
#' @seealso \code{\link[grDevices]{col2rgb}}, \code{\link[grDevices]{rgb}}
#' 
#' @importFrom grDevices col2rgb rgb
#' @seealso \code{\link{alpha2opaque}}
#' @export

col2alpha <- function(col, alpha = 0.5) {
  rgb(t(col2rgb(col))/255, alpha = alpha)
}

