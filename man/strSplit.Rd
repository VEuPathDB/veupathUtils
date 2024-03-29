% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-string.R
\name{strSplit}
\alias{strSplit}
\title{Efficient Vectorized String Split}
\usage{
strSplit(str, pattern, ncol = 2, index = 1, fixed = TRUE)
}
\arguments{
\item{str}{Character vector or list of strings to split}

\item{pattern}{Character string or regular expression to split str on}

\item{ncol}{Number of anticipated output values/ columns after splitting}

\item{index}{Number indicating index of value/ column of interest}

\item{fixed}{Boolean indicating if \code{pattern} is a literal string (TRUE)
or regular expression (FALSE)}
}
\value{
Character vector of substrings
}
\description{
This function efficiently splits strings by a vectorized approach.
}
