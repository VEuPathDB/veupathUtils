% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-cut.R
\name{cut_width}
\alias{cut_width}
\title{Discretise numeric data into equal ranges}
\usage{
cut_width(
  x,
  width,
  center = NULL,
  boundary = NULL,
  closed = c("right", "left"),
  ...
)
}
\arguments{
\item{x}{numeric vector}

\item{width}{The bin width.}

\item{center, boundary}{Specify either the position of edge or the center of
a bin. Since all bins are aligned, specifying the position of a single bin
(which doesn't need to be in the range of the data) affects the location of
all bins. If not specified, uses the "tile layers algorithm", and sets
the boundary to half of the binwidth.

To center on integers, \code{width = 1} and \code{center = 0}.
\code{boundary = 0.5}.}

\item{closed}{One of \code{"right"} or \code{"left"} indicating whether right
or left edges of bins are included in the bin.}

\item{...}{
  Arguments passed on to \code{\link[base:cut]{base::cut.default}}
  \describe{
    \item{\code{breaks}}{either a numeric vector of two or more unique cut points or a
    single number (greater than or equal to 2) giving the number of
    intervals into which \code{x} is to be cut.}
    \item{\code{labels}}{labels for the levels of the resulting category.  By default,
    labels are constructed using \code{"(a,b]"} interval notation.  If
    \code{labels = FALSE}, simple integer codes are returned instead of
    a factor.}
    \item{\code{right}}{logical, indicating if the intervals should be closed on
    the right (and open on the left) or vice versa.}
    \item{\code{dig.lab}}{integer which is used when labels are not given.  It
    determines the number of digits used in formatting the break numbers.}
    \item{\code{ordered_result}}{logical: should the result be an ordered factor?}
  }}
}
\description{
This function makes groups of width \code{width}.
}
