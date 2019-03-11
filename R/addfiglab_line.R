#-------------------------------------------------------------------------------
# addfiglab_line: Add a figure label based on margin line 
#-------------------------------------------------------------------------------

#' @name addfiglab_line
#' @title Add a figure label
#' @description Add a figure label based on x and y line values
#' 
#' @param lab Character giving the figure label
#' @param xl Number of lines into the left x margin
#' @param xy Number of lines intot the top y margin
#' @inheritParams line2user
#' @param ... Additional parameters passed to text
#' 
#' @details 
#' Note, 'xpd' is \code{NA} and 'adj' is {c(0, 1)}.
#'
#' @importFrom graphics text par grconvertX grconvertY
#' @export

addfiglab_line <- function(lab, xl = NULL, yl = NULL, outer = FALSE, ...) {
  
  if (is.null(xl)) xl <- ifelse(outer, par('oma')[2], par('mar')[2])
  if (is.null(yl)) yl <- ifelse(outer, par('oma')[3], par('mar')[3])
  
  text(x = line2user(xl, 2, outer = outer), 
       y = line2user(yl, 3, outer = outer), 
       lab, 
       xpd = NA, 
       adj = c(0, 1), ...)
  
}