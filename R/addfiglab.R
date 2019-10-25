#-------------------------------------------------------------------------------
# addfiglab: Add a figure label in topleft-most corner
#-------------------------------------------------------------------------------

#' @name addfiglab
#' @title Add a figure label in topleft-most corner
#' @description Add a figure label in topleft-most corner
#' 
#' @param lab Character giving the figure label
#' @param units Character giving the device units to use -- will typically be
#' 'ndc' or 'nfc'
#' @inheritParams graphics::text
#' @param ... Additional parameters passed to text
#' 
#' @details 
#' Note, 'xpd' is \code{NA} and 'adj' is {c(0, 1)}.
#'
#' @importFrom graphics text par grconvertX grconvertY
#' @export

addfiglab <- function(lab, font = 2, units = "ndc", ...) {
  
  text(x = grconvertX(0, units, "user"), 
       y = grconvertY(1, units, "user"), 
       lab, 
       xpd = NA, 
       adj = c(0, 1), 
       font = font,
       ...)
  
}