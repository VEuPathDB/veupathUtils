% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-Megastudy.R
\docType{class}
\name{StudySpecificVocabulariesByVariable-class}
\alias{StudySpecificVocabulariesByVariable-class}
\alias{StudySpecificVocabulariesByVariable}
\title{Study Specific Vocabularies By Variable}
\description{
A class to specify expected values per study for some variable
of interest.
}
\section{Slots}{

\describe{
\item{\code{studyIdColumnName}}{A string specifying the name of the column in the vocab data table that contains the study id}

\item{\code{variableSpecColumnName}}{A string specifying the name of the column in the vocab data table that contains the variable vocabulary values}

\item{\code{studyVocab}}{A data.table with columns studyIdColumnName and variableSpecColumnName that specifies expected vocabularies for each study}
}}

