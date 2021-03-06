% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate-rq.R
\name{calculate_rq}
\alias{calculate_rq}
\title{Calculate RQ from ddCT data.}
\usage{
calculate_rq(ddCT, genes)
}
\arguments{
\item{ddCT}{ddCT qPCR data as a \code{data.table} (i.e., output of \code{calculate_ddct()}).}

\item{genes}{A character vector listing genes corresponding to a subset of the columns of each \code{data.table} object.}
}
\value{
Relative quantification (RQ) data as a \code{data.table}.
}
\description{
Calculate relative quantification (RQ) from delta delta CT.
}
\author{
Ayush Noori
}
