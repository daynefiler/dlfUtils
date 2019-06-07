#' @name checkFuncs
#' @title Common parameter checks
#' @description Common function parameter checks
#' @param x R object
NULL

#' @describeIn checkFuncs length 1 character
#' @export

l1chr <- function(x) length(x) == 1 && is.character(x)

#' @describeIn checkFuncs length 1 numeric
#' @export

l1num <- function(x) length(x) == 1 && is.numeric(x)

#' @describeIn checkFuncs length 1 integer
#' @export

l1int <- function(x) length(x) == 1 && is.integer(x)

#' @describeIn checkFuncs length 1 logical
#' @export

l1log <- function(x) length(x) == 1 && is.logical(x)
