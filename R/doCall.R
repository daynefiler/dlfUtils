#' @name doCall
#' @title Wrapper for do.call
#' @description Wrapper for 'do.call' with the shorthand for additional args
#' @inheritParams base::do.call
#' @param ... Additional named arguments to be included in 'args'
#' @param warnDup logical 
#' @details 
#' When arguments are duplicated between 'args' and '...' preference is given
#' to '...' (with a warning when warnDup = TRUE, the default).
#' @return The result of the (evaluated) function call.
#' @examples
#' demolist <- list(a = 1:10, b = "hello")
#' demofunc <- function(a, b, c) list(a, b, c)
#' ## The following errors
#' ## do.call(demofunc, c(demolist, c = 1:2)) 
#' do.call(demofunc, c(demolist, list(c = 1:2)))
#' doCall(demofunc, demolist, c = 1:2)
#' @export

doCall <- function(f, args, ..., quote = FALSE, envir = parent.frame(), 
                   warnDup = TRUE) {
  stopifnot(is.list(args))
  stopifnot(is.logical(warnDup) && length(warnDup) == 1)
  stopifnot(is.function(f))
  addArgs <- list(...)
  if (any(names(args) %in% names(addArgs))) {
    if (warnDup) {
      warning("Duplicate arguments given -- arguments given to '...' used.")
    }
    args[names(addArgs)] <- NULL
  }
  do.call(f, c(args, addArgs), quote = quote, envir = envir)
}
