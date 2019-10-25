##----------------------------------------------------------------------------##
## logSumExp: get posterior probabilities for a vector of log-likelihoods
##----------------------------------------------------------------------------##

#' @name logSumExp
#' @title Get posterior probabilities for a vector of log-likelihoods
#' 
#' @param l Numeric vector of log-likelihoods
#' 
#' @export

logSumExp <- function(l) exp(l - (log(sum(exp(l - max(l)))) + max(l)))