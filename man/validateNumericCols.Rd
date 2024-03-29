% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-numeric.R
\name{validateNumericCols}
\alias{validateNumericCols}
\title{Validate numeric columns}
\usage{
validateNumericCols(x, cols, ...)
}
\arguments{
\item{x}{list, data.frame, array, data.table}

\item{cols}{vector of column names or column indices. NAs will be removed.}
}
\value{
given column names
}
\description{
Given a vector of column names or indices, this function ensures all
referenced columns are numeric
}
