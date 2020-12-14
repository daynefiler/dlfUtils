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

addfiglab <- function(lab, font = 2, units = "ndc", cex = 1, ...) {
  wadj <- strwidth(lab, units = "figure", cex = cex, font = font)
  wadj <- grconvertX(wadj, from = "nfc", to = units) 
  wadj <- wadj - grconvertX(0, from = "nfc", to = units)
  hadj <- strheight(lab, units = "figure", cex = cex, font = font)
  hadj <- grconvertY(hadj, from = "nfc", to = units)
  hadj <- hadj - grconvertY(0, from = "nfc", to = units)
  text(x = grconvertX(0 + wadj, units, "user"),
       y = grconvertY(1 - hadj, units, "user"),
       lab,
       xpd = NA,
       adj = c(0.5, 0.5),
       font = font,
       cex = cex,
       ...)
}
