% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-Megastudy.R
\docType{class}
\name{Megastudy-class}
\alias{Megastudy-class}
\alias{Megastudy}
\title{Megastudy}
\description{
A class to encapsulate everything we need for our special handling
of 'megastudies' in EDA. Currently that is imputing zeroes on tall data
given that each (sub-)study has different expected vocabularies for
that data.
}
\section{Slots}{

\describe{
\item{\code{data}}{A data.table}

\item{\code{ancestorIdColumns}}{A character vector of column names representing parent entities of the recordIdColumn.}

\item{\code{studySpecificVocabularies}}{veupathUtils::StudySpecificVocabulariesByVariableList}

\item{\code{collectionIds}}{A data.table including collection ids and any variables of interest for the collection entity.
If none provided, the collection ids will be inferred from those present in \code{data}.}
}}

