##----------------------------------------------------------------------------##
## calcMCC: Calculate the Matthew's correlation coefficient
##----------------------------------------------------------------------------##

#' @name calcMCC
#' @title Calculate the Matthew's correlation coefficient
#' 
#' @param tp Number of true positives
#' @param tn Number of true negatives
#' @param fp Number of false positives
#' @param fn Number of false negatives
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Matthews_correlation_coefficient}
#' 
#' @export

calcMCC <- function(tp, tn, fp, fn) {
  tp <- as.numeric(tp)
  tn <- as.numeric(tn)
  fp <- as.numeric(fp)
  fn <- as.numeric(fn)
  (tp*tn - fp*fn)/(sqrt((tp + fp)*(tp + fn)*(tn + fp)*(tn + fn)))
}