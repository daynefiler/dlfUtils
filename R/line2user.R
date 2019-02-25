##----------------------------------------------------------------------------##
## line2user: Get margin line location in usr coordinates
##----------------------------------------------------------------------------##

#' @name line2user
#' @title Get margin line location in usr coordinates
#' 
#' @param line the number of lines into the margin 
#' @param side an integer specifying which side of the plot: 
#' 1=below, 2=left, 3=above and 4=right.
#' @param outer a logical value indicating whether line refers to the outer 
#' plot margin, rather than the standard plot margin
#' 
#' @details 
#' Code credit: \url{https://stackoverflow.com/a/30835971/1623354}; modification
#' for 'outer' parameter added by DLF. 
#' 
#' 
#' @importFrom graphics grconvertX grconvertY par
#' @export

line2user <- function(line, side, outer = FALSE) {
  unit <- if (outer) "nic" else "npc"
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(c(0, lh), 'inches', unit))
  y_off <- diff(grconvertY(c(0, lh), 'inches', unit))
  switch(side,
         `1` = grconvertY(-line * y_off, unit, 'user'),
         `2` = grconvertX(-line * x_off, unit, 'user'),
         `3` = grconvertY(1 + line * y_off, unit, 'user'),
         `4` = grconvertX(1 + line * x_off, unit, 'user'),
         stop("Side must be 1, 2, 3, or 4", call.=FALSE))
}