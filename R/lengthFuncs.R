##----------------------------------------------------------------------------##
## Length functions
##----------------------------------------------------------------------------##

#' @name lengthFuncs
#' @title Common length calculations
#' @description Common length calculations
#' @param x R object
NULL

#' @describeIn lengthFuncs length(unique(x))
#' @export

lu <- function(x) length(unique(x))

#' @describeIn lengthFuncs sum(x, na.rm = TRUE)
#' @export

lw <- function(x) sum(x, na.rm = TRUE)