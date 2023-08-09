#this first isnt really specific to any class
#just putting it here bc the megastudy introduced the need and i dont see an obviously better spot

#' VariableSpec as String
#' 
#' This function returns a string representation of a VariableSpec. By
#' default it assumes the VariableSpec is available in a slot called
#' `variableSpec`.
#' 
#' @param object An object containing a veupathUtils::VariableSpec
#' @return character
#' @export
setGeneric("getVariableSpecAsString", 
  function(object) standardGeneric("getVariableSpecAsString"),
  signature = "object"
)

#' @export
setMethod('getVariableSpecAsString', signature('ANY'), function(object) {
  if (!'variableSpec' %in% slotNames(object)) stop("Specified object does not have a `variableSpec` slot.")

  return(getColName(object@variableSpec))
})

#' @export 
setMethod('getVariableSpecAsString', signature('StudySpecificVocabulariesByVariable'), function(object) {
  return(getVariableSpecAsString(object[[1]]))
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

# TODO maybe call this getDTWithImputedZeroes?

#' Impute Zeroes (on tall data)
#' 
#' This function returns a data.table which has explicit zero values
#' for all expected categories of some variable of interest in a megastudy.
#' 
#' @param object veupathUtils::Megastudy
#' @return data.table
#' @include class-VariableMetadata.R
#' @export
setGeneric("imputeZeroes", 
  function(object, variables) standardGeneric("imputeZeroes"),
  signature = c("object", "variables")
)

#' @importFrom digest digest
#' @export
setMethod('imputeZeroes', signature = c('Megastudy', 'VariableMetadataList'), function (object, variables) {
 
  weightingVariablesMetadata <- findWeightingVariablesMetadata(variables)
  if (is.null(weightingVariablesMetadata)) return(object@data)

  .dt <- object@data
  vocabs <- object@studySpecificVocabularies
  if (length(weightingVariablesMetadata) > 1) stop("Megastudy class does not yet support imputing zeroes when there is more than one weighting variable present.")
  weightingVarColName <- getColName(weightingVariablesMetadata[[1]]@variableSpec)
  # TODO expand this to allow more than one if theyre all in the same entity and have the same weighting var spec
  if (length(vocabs) > 1) stop("Megastudy class does not yet support imputing zeroes when there is more than one study specific vocabulary present.")
  studyIdColName <- getStudyIdColumnName(vocabs[[1]])
  varSpecColName <- getVariableSpecColumnName(vocabs[[1]])
  ancestorIdColumns <- object@ancestorIdColumns
  # TODO fxn to find ancestor id for entity matching varspec, then remove that from ancestor ids
  varSpecEntityIdColName <- findAncestorIdForVariableSpec(vocabs[[1]]@variableSpec)
  upstreamEntityIdColNames <- ancestorIdColumns[!ancestorIdColumns %in% varSpecEntityIdColName]

  # for upstream entities data
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
  add.dt[[varSpecEntityIdColName]] <- apply(add.dt[, c(ancestorIdColumns[!ancestorIdColumns %in% varSpecEntityIdColName], varSpecColName), with=FALSE], 1, digest::digest, algo='md5')
  # make the dt to rbind to the original .dt
  # TODO make sure cols are ordered the same?
  # TODO what about sample.sex/ other vars on the entity of interest? what value do they get NA?
  .dt2 <- rbind(.dt, add.dt)
  # combine
  .dt <- rbind(.dt, .dt2)

  return(.dt)
})