% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method-correlation.R
\name{selfCorrelation}
\alias{selfCorrelation}
\title{Self Correlation}
\usage{
selfCorrelation(
  data,
  method = c("spearman", "pearson", "sparcc"),
  format = c("ComputeResult", "data.table"),
  verbose = c(TRUE, FALSE),
  ...
)
}
\arguments{
\item{data}{first dataset. A data.table}

\item{method}{string defining the type of correlation to run. The currently supported values are 'spearman','pearson' and 'sparcc'}

\item{format}{string defining the desired format of the result. The currently supported values are 'data.table' and 'ComputeResult'.}

\item{verbose}{boolean indicating if timed logging is desired}
}
\value{
ComputeResult object
}
\description{
This function returns correlation coefficients for variables in one dataset against itself
}
