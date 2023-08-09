#' get a VariableSpec
#' 
#' This function returns a string representation of a VariableSpec. By
#' default it assumes the VariableSpec is available in a slot called
#' `variableSpec`.
#' 
#' @param object An object containing a veupathUtils::VariableSpec
#' @return character
#' @export
setGeneric("getVariableSpec", 
  function(object) standardGeneric("getVariableSpec"),
  signature = "object"
)

#' @export
setMethod('getVariableSpec', signature('ANY'), function(object) {
  if (!'variableSpec' %in% slotNames(object)) stop("Specified object does not have a `variableSpec` slot.")

  return(object@variableSpec)
})

#' @export 
setMethod('getVariableSpec', signature('StudySpecificVocabulariesByVariable'), function(object) {
  return(veupathUtils::getVariableSpec(object[[1]]))
})

#' StuydIdColName as String
#' 
#' This function returns the studyIdColName from an StudySpecificVocabulary
#' 
#' @param object veupathUtils::StudySpecificVocabulary
#' @return character
#' @export
setGeneric("getStudyIdColumnName", 
  function(object) standardGeneric("getStudyIdColumnName"),
  signature = "object"
)

#' @export
setMethod('getStudyIdColumnName', signature('StudySpecificVocabulary'), function(object) {
  return(object@studyIdColumnName)
})

#' @export
setMethod('getStudyIdColumnName', signature('StudySpecificVocabulariesByVariable'), function(object) {
  #since we validate theyre all the same, can just take the first
  return(object[[1]]@studyIdColumnName)
})

#' @export 
setMethod('getStudyIdColumnName', signature('StudySpecificVocabulariesByVariableList'), function(object) {
  return(unlist(lapply(as.list(object), veupathUtils::getStudyIdColumnName)))
})

#' VarSpecColName as String
#' 
#' This function returns the variableSpec from an StudySpecificVocabulary
#' 
#' @param object veupathUtils::StudySpecificVocabulary
#' @return character
#' @export
setGeneric("getVariableSpecColumnName", 
  function(object) standardGeneric("getVariableSpecColumnName"),
  signature = "object"
)

#' @export
setMethod('getVariableSpecColumnName', signature('StudySpecificVocabulary'), function(object) {
  return(veupathUtils::getColName(object@variableSpec))
})

#' @export
setMethod('getVariableSpecColumnName', signature('StudySpecificVocabulariesByVariable'), function(object) {
  #since we validate theyre all the same, can just take the first
  return(veupathUtils::getColName(object[[1]]@variableSpec))
})

#' @export 
setMethod('getVariableSpecColumnName', signature('StudySpecificVocabulariesByVariableList'), function(object) {
  return(unlist(lapply(as.list(object), veupathUtils::getVariableSpecColumnName)))
})

#' @export
setGeneric("getEntityId", 
  function(object) standardGeneric("getEntityId"),
  signature = "object"
)

#' @export
setMethod('getEntityId', signature('VariableSpec'), function(object) {
  return(object@entityId)
})

#' @export
setMethod('getEntityId', signature('StudySpecificVocabulary'), function(object) {
  return(veupathUtils::getEntityId(object@variableSpec))
})

#' @export
setMethod('getEntityId', signature('StudySpecificVocabulariesByVariable'), function(object) {
  #since we validate theyre all the same, can just take the first
  return(veupathUtils::getEntityId(object[[1]]@variableSpec))
})

#' as.data.table
#' 
#' This function returns a data.table representation of 
#' StudySpecificVocabulary or StudySpecificVocabularyByVariable
#' 
#' @return data.table
#' @export 
as.data.table <- makeGeneric('as.data.table', data.table::as.data.table)

#' @export
setMethod('as.data.table', signature('StudySpecificVocabulary'), function(x) {
  .dt <- data.table::data.table('study'=x@study, 'variable'=x@vocabulary)
  names(.dt) <- c(x@studyIdColumnName, getColName(x@variableSpec))

  return(.dt)
})

#' @export 
setMethod('as.data.table', signature('StudySpecificVocabulariesByVariable'), function(x) {
  return(purrr::reduce(lapply(as.list(x), veupathUtils::as.data.table), rbind))
})

#should this be an s4 method?
findAncestorIdForVariableSpec <- function(varSpec, ancestorIdColumns) {
  if (!inherits(varSpec, 'VariableSpec')) stop("The first argument must be of the S4 class `VariableSpec`.")

  return(ancestorIdColumns[grepl(varSpec@entityId, ancestorIdColumns)])
}


#' Impute Zeroes (on tall data)
#' 
#' This function returns a data.table which has explicit zero values
#' for all expected categories of some variable of interest in a megastudy.
#' 
#' @param object veupathUtils::Megastudy
#' @return data.table
#' @include class-VariableMetadata.R
#' @export
setGeneric("getDTWithImputedZeroes", 
  function(object, variables) standardGeneric("getDTWithImputedZeroes"),
  signature = c("object", "variables")
)

#' @importFrom digest digest
#' @export
setMethod('getDTWithImputedZeroes', signature = c('Megastudy', 'VariableMetadataList'), function (object, variables) {
 
  weightingVariablesMetadata <- findWeightingVariablesMetadata(variables)
  if (is.null(weightingVariablesMetadata)) return(object@data)

  .dt <- object@data
  vocabs <- object@studySpecificVocabularies

  if (length(weightingVariablesMetadata) > 1) stop("Megastudy class does not yet support imputing zeroes when there is more than one weighting variable present.")
  if (length(vocabs) > 1) {
    varSpecsWithStudyVocabs <- VariableSpecList(S4Vectors::SimpleList(lapply(as.list(vocabs), getVariableSpec)))
    variableMetadataForStudyVocabVariables <- findVariableMetadataFromVariableSpec(variables, varSpecsWithStudyVocabs)
    weightingVarSpecsForStudyVocabVariables <- findWeightingVariableSpecs(variableMetadataForStudyVocabVariables)
    weightingVarColName <- unlist(lapply(weightingVarSpecsForStudyVocabVariables, veupathUtils::getColName))
    if (length(weightingVarColName) > 1) {
      stop("All study vocabularies must belong to variables on the same entity using the same weighting variable.")
    }
  } else {
    weightingVarColName <- veupathUtils::getColName(findVariableMetadataFromVariableSpec(variables, vocabs[[1]]@variableSpec)[[1]]@weightingVariableSpec)
  }

  studyIdColNames <- getStudyIdColumnName(vocabs)
  varSpecColNames <- getVariableSpecColumnName(vocabs)
  allEntityIdColumns <- object@ancestorIdColumns
  # this works bc we validate all vocabs must be on the same entity
  varSpecEntityIdColName <- findAncestorIdColumnNameForVariableSpec(vocabs[[1]]@variableSpec, ancestorIdColumns)
  if (!all(unlist(getHasStudyDependentVocabulary(findVariableMetadataFromEntityId(variables, vocabs[[1]]@variableSpec@entityId))))) {
    stop("Not all variables on the entity associated with the study vocabulary have study vocabularies.")
  }
  upstreamEntityIdColNames <- ancestorIdColumns[!ancestorIdColumns %in% varSpecEntityIdColName]

  # for upstream entities data
  # TODO make sure add.dt is done once for each vocab, and the list of data.tables merged together to include all combinations
  combinations.dt <- unique(.dt[, -c(weightingVarColName, varSpecColName), with=FALSE])
  # for the var of interest data
  ancestors.dt <- unique(.dt[, c(ancestorIdColumns), with=FALSE])
  vocabs.dt <- merge(ancestors.dt, veupathUtils::as.data.table(vocabs), by=studyIdColName, allow.cartesian=TRUE)
  present.dt <- unique(.dt[, c(ancestorIdColumns, varSpecColName), with=FALSE])
  # assume if a value was explicitly filtered against that its not in the vocab
  add.dt <- vocabs.dt[!present.dt, on=c(ancestorIdColumns, varSpecColName)]
  add.dt <- merge(add.dt, combinations.dt, by=upstreamEntityIdColNames)
  add.dt[[weightingVarColName]] <- 0
  #make impossibly unique ids
  add.dt[[varSpecEntityIdColName]] <- apply(add.dt[, c(upstreamEntityIdColNames, varSpecColName), with=FALSE], 1, digest::digest, algo='md5')
  .dt <- rbind(.dt, add.dt)

  return(.dt)
})