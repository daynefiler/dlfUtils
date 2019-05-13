##----------------------------------------------------------------------------##
## plotPal: plot a palette of colors
##----------------------------------------------------------------------------##

#' @name plotPal
#' @title Plot palette of colors
#' 
#' @param col The colors to plot; plots current value of palette() if NULL
#' @export

plotPal <- function(col = palette()) {
  n <- length(col)
  par(mar = c(3, 0, 0, 0) + 0.1)
  plot.new()
  plot.window(xlim = c(1, n), ylim = c(-0.1, 0.1))
  abline(v = 1:n, lty = "dashed", col = "lightgray")
  points(x = 1:n, y = rep(0, n), col = col, pch = 16, cex = 2)
  axis(side = 1)
}