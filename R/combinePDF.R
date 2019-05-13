##----------------------------------------------------------------------------##
## combinePDF: combine list of pdf files 
##----------------------------------------------------------------------------##

#' @name combinePDF
#' @title Combine list of pdf files
#' 
#' @param input Vector with paths to pdf files 
#' @param output Character with path to output pdf
#' 
#' @details 
#' This function utilizes ghostscript, and will not work without a Ghostscript
#' installation
#' 
#' @note 
#' The strength of this approach is preserving hyperlinks.
#' 
#' @export

combinePDF <- function(input, output) {
  gsCall <- "gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=%s %s"
  system(sprintf(gsCall, output, paste(input, collapse = " ")))
}