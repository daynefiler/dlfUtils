#' @name matToPyArray
#' @title cat code to create numpy array
#' @param X matrix
#' @return NULL
#' @export

matToPyArray <- function(X) {
  stopifnot(is.matrix(X))
  bracCollapse <- function(x) sprintf("[%s]", paste(x, collapse = ","))
  rows <- bracCollapse(apply(X, 1, bracCollapse))
  cat(sprintf("np.array(%s)", rows))
  NULL
}