##----------------------------------------------------------------------------##
## alpha2opaque: convert transparent color to opaque equivalent
##----------------------------------------------------------------------------##

#' @name alpha2opaque
#' @title Convert transparent color to opaque equivalent
#' 
#' @param c1 transparent color, as defined by any of the three R specifications 
#' @param c2 opaque background color, defaults to "white"
#' 
#' @seealso \code{\link[grDevices]{col2rgb}}, \code{\link[grDevices]{rgb}}
#' 
#' @importFrom grDevices col2rgb rgb
#' @seealso \code{\link{col2alpha}}
#' @export

alpha2opaque <- function(c1, c2 = "#FFFFFF") {
  c1rgb <- col2rgb(c1, alpha = TRUE)
  c2rgb <- col2rgb(c2, alpha = TRUE)
  if (c2rgb[4] < 255) stop("'c2' must be fully opaque.")
  c3rgb <- c2rgb + (c1rgb - c2rgb)*(c1rgb[4]/255)
  rgb(t(c3rgb), maxColorValue = 255)
}
