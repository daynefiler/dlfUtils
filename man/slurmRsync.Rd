% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slurmRsync.R
\name{slurmRsync}
\alias{slurmRsync}
\title{Create slurm job array for rsync of multiple files}
\usage{
slurmRsync(spath, dpath, rel = TRUE, relTo = dirname(spath), rsyncOpts = "avP")
}
\arguments{
\item{spath}{character, the source to be copied}

\item{dpath}{character of length 1, the destination location}

\item{rel}{logical of length 1, should the rsync be done relatively (using
the -R or --relative option)?}

\item{relTo}{when rel = TRUE, the path to copy relative to}
}
\description{
Create slurm job array for rsync of multiple files
}
\details{
Submits a slurm array for rsync of multiple files.
}
