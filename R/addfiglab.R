#-------------------------------------------------------------------------------
# addfiglab: Add a figure label
#-------------------------------------------------------------------------------

#' @name addfiglab
#' @title Add a figure label
#' @description Add a figure label based on x and y line values
#' 
#' @param lab Character giving the figure label
#' @param xl Number of lines into the left x margin
#' @param xy Number of lines intot the top y margin
#' @param cex A numerical value giving the amount by which figure label 
#' should be magnified relative to the default.
#' @param ... Additional parameters passed to text
#' 
#' @details 
#' Note, 'xpd' is \code{NA} and 'adj' is {c(0, 1)}.
#'
#' @importFrom graphics text par
#' @export

addfiglab <- function(lab, xl = par()$mar[2], yl = par()$mar[3], cex = 1, ...) {
  
  text(x = line2user(xl, 2), y = line2user(yl, 3), 
       lab, xpd = NA, font = 2, cex = cex, adj = c(0, 1), ...)
  
}