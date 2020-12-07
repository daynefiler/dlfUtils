##----------------------------------------------------------------------------##
## fig2minipage: build minipage text
##----------------------------------------------------------------------------##

#' @name fig2minipage
#' @title Modify the alpha value for given color vector
#' 
#' @param path file path to pdf for include graphics 
#' @param width numeric of length 1, between 0 and 1 -- passed to minipage width
#' @param newline logical of length 1, when FALSE a '\%' is appended to the end
#' minipage call
#' @param adjWidth logical of length 1, when TRUE [width=\\linewidth] is added
#' to the includegraphics call
#' @param ... added for flexibility, not passed to anything
#' 
#' @note NOT exported
#' @seealso \code{\link{stitchPlots}}
#' @export

fig2minipage <- function(path, width, newline = FALSE, adjWidth = FALSE, ...) {
  path <- normalizePath(path)
  stopifnot(file.exists(path))
  stopifnot(width > 0 && width <= 1)
  beginMinipage <- sprintf("\\begin{minipage}{%s\\linewidth}", width)
  endMinipage <- sprintf("\\end{minipage}%s", ifelse(newline, "", "%"))
  adj <- if (adjWidth) "width=\\linewidth" else ""
  ncldGraph <- sprintf("\\includegraphics[%s]{%s}", adj, path)
  minipage <- c(beginMinipage, "\\centering", ncldGraph, endMinipage)
  minipage
}

##----------------------------------------------------------------------------##
## stitchPlots: combine PDFs into a single plot using minipages
##----------------------------------------------------------------------------##

#' @name stitchPlots
#' @title Combine PDFs into a single plot using minipages
#' @description Combine PDFs into a single plot using minipages
#' @param figLst list of "figures" -- see details
#' @param width numeric of length 1, width of the output pdf in inches
#' @param height numeric of length 1, height of the output pdf in inches
#' @param figname character of length 1, output file name/path
#' 
#' @details
#' Each item in 'figLst' points to the input PDF and provides details about the
#' minipage environment. Each item should include
#' \itemize{
#' \item[path] character of length 1, the path to the PDF/image, passed to 
#' includegraphics
#' \item[width] numeric of length 1, the width passed to minipage as a fraction 
#' of linewidth (0-1]
#' \item[newline] logical of length 1, should the minipage be followed by a 
#' newline or escaped using a '\%'; newline when TRUE, '%' when FALSE
#' \item[adjWidth] logical of length 1, should the images be adjusted to the
#' linewidth of the bounding minipage
#' }
#' 
#' Figures are all centered, and included in the list order. PDF compiled by
#' \code{\link[tinytex]{xelatex}} in a temporary directory.
#' 
#' @seealso \code{\link[tinytex]{xelatex}}, \code{\link{fig2minipage}}
#' @importFrom tinytex xelatex
#' @export

stitchPlots <- function(figLst, width, height, figname = "Rplot.pdf") {
  if (!grepl(".pdf$", figname)) figname <- paste0(figname, ".pdf")
  geoFrm <- "\\usepackage[papersize={%sin,%sin},margin=0in]{geometry}"
  geo <- sprintf(geoFrm, width, height)
  fig <- unlist(lapply(figLst, do.call, what = fig2minipage))
  doc <- c("\\documentclass{article}",
           "\\usepackage{graphicx}",
           geo,
           "\\setlength{\\parindent}{0pt}",
           "\\setlength{\\parskip}{0pt}",
           "\\begin{document}",
           "\\centering",
           fig,
           "\\end{document}")
  tmpdir <- tempdir()
  ofl <- file.path(tmpdir, "temp.tex")
  cat(doc, sep = "\n", file = ofl)
  xelatex(ofl, pdf_file = figname)
}

