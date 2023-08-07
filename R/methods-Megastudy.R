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
setGeneric("getStudyIdColName", 
  function(object) standardGeneric("getStudyIdColName"),
  signature = "object"
)

#' @export
setMethod('getStudyIdColName', signature('StudySpecificVocabulary'), function(object) {
  return(object@studyIdColName)
})

#' @export
setMethod('getStudyIdColName', signature('StudySpecificVocabulariesByVariable'), function(object) {
  #since we validate theyre all the same, can just take the first
  return(object[[1]]@studyIdColName)
})

#' StudySpecifcVocabulary as data.table
#' 
#' This function returns a data.table representation of StudySpecificVocabulary
#' 
#' @return data.table
#' @export 
makeGeneric('as.data.table', as.data.table)

#' @export
setMethod('as.data.table'), signature('StudySpecificVocabulary', function(object) {
  .dt <- data.table('study'=object@study, 'variable'=object@vocabulary)
  # TODO how to name the study col? do we need a studyId slot in this class?
  names(.dt) <- c('study', getColName(object@variableSpec))

  return(.dt)
})

#' @export 
setMethod('as.data.table', signature('StudySpecificVocabularyByVariable'), function(object) {
  return(purrr::reduce(lapply(as.list(object), as.data.table), rbind))
})

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
  signature("object", "variables")
)

#' @export
setMethod('imputeZeroes', signature('Megastudy', 'VariableMetadata'), function (object, variables) {
 
  weightingVariablesMetadata <- findWeightingVariablesMetadata(variables)
  if (is.null(weightingVariablesMetadata)) return(object@data)

  .dt <- object@data
  vocabs <- object@studySpecificVocabularies
  weightingVariablesMetadata <- findWeightingVariablesMetadata(variables)
  if (length(weightingVariablesMetadata) > 1) stop("Megastudy class does not yet support imputing zeroes when there is more than one weighting variable present.")
  weightingVarColName <- getColName(findWeightingVariablesMetadata(variables)[[1]])
  # TODO expand this to allow more than one if theyre all in the same entity and have the same weighting var spec
  if (length(vocabs) > 1) stop("Megastudy class does not yet support imputing zeroes when there is more than one study specific vocabulary present.")
  studyIdColName <- getStudyIdColName(vocabs[[1]])
  varSpecColName <- getVarSpecColName(vocabs[[1]])

  # for upstream entities data
  combinations.dt <- unique(.dt[, -c(get(weightingVarColName), get(varSpecColName)), with=F])
  # for the var of interest data
  vocabs.dt <- as.data.table(vocabs)
  present.dt <- unique(.dt[, c(studyIdColName, varSpecColName), with=FALSE])
  add.dt <- vocabs.dt[!present.dt, on=.(get(studyIdColName), get(varSpecColName))]
  # make the dt to rbind to the original .dt
  # TODO make sure we dont need an arg to force the left side to keep all rows and the by key is correct, etc. this isnt tested yet.
  .dt2 <- merge(combinations.dt, add.dt, by=get(studyIdColName))
  .dt2[[weightingVarColName]] <- 0
  # combine
  .dt <- rbind(.dt, .dt2)

  return(.dt)
})