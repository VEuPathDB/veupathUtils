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

  if (!!length(object@collectionIdColumn)) {
    
    if (length(object@collectionIdColumn) != 1) {
      msg <- paste("Collection ID column must have a single value.")
      errors <- c(errors, msg)
    }

    if (!object@collectionIdColumn %in% ancestor_id_cols) {
      msg <- paste("Collection ID column must be an ancestor ID column")
      errors <- c(errors, msg)
    }

    if (!!length(object@collectionsDT)) {
      if (!any(object@collectionsDT[[object@collectionIdColumn]] %in% df[[object@collectionIdColumn]])) {
        msg <- paste("Collection IDs not found in data.frame. At least some of the provided collection IDs should be present in the data.frame.")
        errors <- c(errors, msg)
      }
    }
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
#' @slot ancestorIdColumns A character vector of column names representing parent entities of the recordIdColumn.
#' @slot studySpecificVocabularies veupathUtils::StudySpecificVocabulariesByVariableList
#' @slot collectionIdColumn The name of the column in the data.frame that contains the collection ids. 
#' The collectionId should also be a member of ancestorIdColumns.
#' @slot collectionIds A character vector of collection ids we expect. If none provided, they will be inferred from those present.
#' 
#' @name Megastudy-class
#' @rdname Megastudy-class
#' @export 
Megastudy <- setClass("Megastudy", representation(
    data = 'data.table',
    ancestorIdColumns = 'character',
    studySpecificVocabularies = 'StudySpecificVocabulariesByVariableList',
    collectionIdColumn = 'character',
    collectionsDT = 'data.frame'
), validity = check_megastudy)