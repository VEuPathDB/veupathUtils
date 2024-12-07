% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-Collections.R
\docType{class}
\name{Collection-class}
\alias{Collection-class}
\title{Data Collection}
\description{
This class represents a collection of related data. That could be
all of the abundance data for all samples in a dataset at a particular taxonomic rank,
or all pathway abundance data for all samples in a dataset, or something else. The
primary requirement for a collection is that the values of all variables in the
collection can be expressed on the same theoretical range.
}
\section{Slots}{

\describe{
\item{\code{name}}{The human-readable name of the collection. This can be anything that helps to identify the collection uniquely to the user.}

\item{\code{data}}{A data.frame of data with samples as rows and variables as columns}

\item{\code{recordIdColumn}}{The name of the column in the data.frame that contains the record id}

\item{\code{ancestorIdColumns}}{A character vector of column names representing parent entities of the recordIdColumn}

\item{\code{imputeZero}}{A logical indicating whether NA/ null values should be replaced with zeros.}

\item{\code{removeEmptyRecordss}}{A logical indicating whether empty (all NA/ zero) records should be removed.}
}}

