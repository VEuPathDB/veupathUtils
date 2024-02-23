check_study_vocabulary <- function(object) {
  errors <- character()

  # the column names should be in the vocabulary
  if (!object@studyIdColumnName %in% names(object@studyVocab)) {
    msg <- paste0("Study ID column '", object@getStudyIdColumnName, "' not found in vocabulary.")
    errors <- c(errors, msg)
  }

  if (!veupathUtils::getColName(object@variableSpec) %in% names(object@studyVocab)) {
    msg <- paste0("Variable spec column '", veupathUtils::getColName(object@variableSpec), "' not found in vocabulary.")
    errors <- c(errors, msg)
  }

  return(if (length(errors) == 0) TRUE else errors)
}

#' Study Specific Vocabularies By Variable
#' 
#' A class to specify expected values per study for some variable
#' of interest.
#' 
#' @slot studyIdColumnName A string specifying the name of the column in the vocab data table that contains the study id
#' @slot variableSpecColumnName A string specifying the name of the column in the vocab data table that contains the variable vocabulary values
#' @slot studyVocab A data.table with columns studyIdColumnName and variableSpecColumnName that specifies expected vocabularies for each study
#' @name StudySpecificVocabulariesByVariable-class
#' @rdname StudySpecificVocabulariesByVariable-class
#' @include class-VariableMetadata.R
#' @export
StudySpecificVocabulariesByVariable <- setClass("StudySpecificVocabulariesByVariable",
  representation = representation(
    studyIdColumnName = 'character',
    variableSpec = 'VariableSpec',
    studyVocab = 'data.table'
  ),
  validity = check_study_vocabulary
)

check_multiple_study_vocabularies_on_same_entity <- function(object) {
  errors <- character()

  if (length(unique(unlist(lapply(as.list(object), getStudyIdColumnName)))) != 1) {
    errors <- c(errors, paste0("All study vocabularies must be able to be identified by the same study entity. Found the following study entities: ", paste(unique(unlist(lapply(as.list(object), getStudyIdColumnName))), collapse = ", ")))
  }

  if (length(unique(unlist(lapply(as.list(object), getEntityId)))) != 1) {
    errors <- c(errors, paste0("All study vocabularies must belong to the same entity. Found the following entities: ", paste(unique(unlist(lapply(as.list(object), getEntityId))), collapse = ", ")))
  }

  return(if (length(errors) == 0) TRUE else errors)
}

#' @export
StudySpecificVocabulariesByVariableList <- setClass("StudySpecificVocabulariesByVariableList",
  contains = "SimpleList",
  prototype = prototype(elementType = "StudySpecificVocabulariesByVariable"),
  validity = check_multiple_study_vocabularies_on_same_entity
)

#this also sets us up for megastudy specific methods in plot.data if it turns out we need them

check_megastudy <- function(object) {
  errors <- character()
  df <- object@data
  ancestor_id_cols <- object@ancestorIdColumns

  if (!!length(ancestor_id_cols)) {
    if (!all(ancestor_id_cols %in% names(df))) {
      msg <- paste("Not all ancestor ID columns are present in data.frame")
      errors <- c(errors, msg)
    }
  } else {
    msg <- paste("Ancestor ID columns are required but not provided.")
    errors <- c(errors, msg)
  }

  return(if (length(errors) == 0) TRUE else errors)
}

#' Megastudy
#' 
#' A class to encapsulate everything we need for our special handling 
#' of 'megastudies' in EDA. Currently that is imputing zeroes on tall data
#' given that each (sub-)study has different expected vocabularies for
#' that data.
#' 
#' @slot data A data.table
#' @slot studySpecificVocabularies veupathUtils::StudySpecificVocabulariesByVariableList
#' 
#' @name Megastudy-class
#' @rdname Megastudy-class
#' @export 
Megastudy <- setClass("Megastudy", representation(
    data = 'data.table',
    ancestorIdColumns = 'character',
    studySpecificVocabularies = 'StudySpecificVocabulariesByVariableList'
), validity = check_megastudy)