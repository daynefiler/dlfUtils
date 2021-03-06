% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twoDimDen.R
\name{twoDimDen}
\alias{twoDimDen}
\title{Create 2d density on log-scale}
\usage{
twoDimDen(x, y, nbin = 100, lims = NULL, freq = FALSE, log = FALSE)
}
\arguments{
\item{x}{the x values}

\item{y}{the y values}

\item{nbin}{the number of bins for the density calculation}

\item{lims}{vector containing the x and y limits c(x1, x2, y1, y2)}

\item{freq}{logical, return frequency? density is returned if FALSE}

\item{log}{character; indicating which axes should be in log scale.}
}
\value{
List with: 'x' and 'y' giving the bin number for x and y; 'z' matrix
giving the frequency/density in each bin; 'xseq' and 'yseq' giving the break
points for the x and y bins
}
\description{
Create 2d density on log-scale
}
