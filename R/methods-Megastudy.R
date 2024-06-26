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
  function(object, ...) standardGeneric("getVariableSpec"),
  signature = "object"
)

#' @export 
setMethod('getVariableSpec', signature('StudySpecificVocabulariesByVariable'), function(object) {
  return(object@variableSpec)
})

#' @export
setMethod('getVariableSpec', signature('ANY'), function(object) {
  return(object@variableSpec)
})

#' @export
setMethod('getVariableSpec', signature('VariableMetadata'), function(object, getCollectionMemberVarSpecs = c("Dynamic", "Never", "Always")) {
  getCollectionMemberVarSpecs <- veupathUtils::matchArg(getCollectionMemberVarSpecs)
  varSpecs <- list(object@variableSpec)

  #if the variable is a collection, then we want to return the member variable specs
  if (object@isCollection && getCollectionMemberVarSpecs %in% c("Dynamic", "Always")) {
    varSpecs <- as.list(object@members)
  } else if (!object@isCollection && getCollectionMemberVarSpecs == "Always") {
    varSpecs <- NULL
  }

  return(varSpecs)
})

#' @export 
setMethod('getVariableSpec', signature('VariableMetadataList'), function(object, getCollectionMemberVarSpecs = c("Dynamic", "Never", "Always")) {
  getCollectionMemberVarSpecs <- veupathUtils::matchArg(getCollectionMemberVarSpecs)
  
  varSpecs <- unlist(lapply(as.list(object), veupathUtils::getVariableSpec, getCollectionMemberVarSpecs))
  if (all(unlist(lapply(varSpecs, is.null)))) {
    return(NULL)
  }

  return(varSpecs)
})

#' StuydIdColName as String
#' 
#' This function returns the studyIdColName from an StudySpecificVocabulariesByVariable
#' 
#' @param object veupathUtils::StudySpecificVocabulariesByVariable
#' @return character
#' @export
setGeneric("getStudyIdColumnName", 
  function(object) standardGeneric("getStudyIdColumnName"),
  signature = "object"
)

#' @export
setMethod('getStudyIdColumnName', signature('StudySpecificVocabulariesByVariable'), function(object) {
  return(object@studyIdColumnName)
})

#' @export 
setMethod('getStudyIdColumnName', signature('StudySpecificVocabulariesByVariableList'), function(object) {
  #works bc of class validation that theyre all the same
  return(veupathUtils::getStudyIdColumnName(object[[1]]))
})

#' VarSpecColName as String
#' 
#' This function returns the variableSpec from an StudySpecificVocabulariesByVariable
#' 
#' @param object veupathUtils::StudySpecificVocabulariesByVariable
#' @return character
#' @export
setGeneric("getVariableSpecColumnName", 
  function(object) standardGeneric("getVariableSpecColumnName"),
  signature = "object"
)

#' @export
setMethod('getVariableSpecColumnName', signature('StudySpecificVocabulariesByVariable'), function(object) {
  #since we validate theyre all the same, can just take the first
  return(veupathUtils::getColName(object@variableSpec))
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
setMethod('getEntityId', signature('StudySpecificVocabulariesByVariable'), function(object) {
  return(veupathUtils::getEntityId(object@variableSpec))
})

#should this be an s4 method?
findEntityIdColumnNameForVariableSpec <- function(varSpec, entityIdColumns) {
  if (!inherits(varSpec, 'VariableSpec')) stop("The first argument must be of the S4 class `VariableSpec`.")
  
  return(entityIdColumns[grepl(varSpec@entityId, entityIdColumns)])
}

findStudyVocabularyByVariableSpec <- function(vocabs, variables, variableSpec) {
  if (!inherits(vocabs, 'StudySpecificVocabulariesByVariableList')) stop("The first argument must be of the S4 class `StudySpecificVocabulariesByVariableList`.")
  if (!inherits(variables, 'VariableMetadataList')) stop("The second argument must be of the S4 class `VariableMetadataList`.")
  if (!inherits(variableSpec, 'VariableSpec')) stop("The third argument must be of the S4 class `VariableSpec`.")

  vocabVariableSpecs <- lapply(as.list(vocabs), veupathUtils::getVariableSpec)
  vocabVariableMetadata <- veupathUtils::findVariableMetadataFromVariableSpec(variables, veupathUtils::VariableSpecList(S4Vectors::SimpleList(vocabVariableSpecs)))
  vocabVariableSpecsAdjustedForVariableCollectionMembers <- veupathUtils::getVariableSpec(vocabVariableMetadata, "Always")
  
  # if we have found variable collection members in the VariableMetadata, need to check if the passed varspec was a member
  # look through the list that includes the members, and if we match one, get the varspec of the parent/ collection
  # use the varspec of the parent/ collection to get the VariableMetadata associated w the entire collection
  # remember, individual members dont have their own VariableMetadata
  if (length(vocabVariableSpecsAdjustedForVariableCollectionMembers) > 0) {
    index <- which(purrr::map(vocabVariableSpecsAdjustedForVariableCollectionMembers, function(x) {veupathUtils::getColName(x)}) == veupathUtils::getColName(variableSpec))
    variableCollectionSpecs <- vocabVariableSpecsAdjustedForVariableCollectionMembers[[index]]
    index <- which(purrr::map(vocabVariableMetadata, function(x) {veupathUtils::getColName(variableCollectionSpecs) %in% unlist(veupathUtils::getColName(x@members))}) == TRUE)
  } else {
    index <- which(purrr::map(vocabVariableSpecs, function(x) {veupathUtils::getColName(x)}) == veupathUtils::getColName(variableSpec))
  }
 
  return(vocabs[[index]])  
}


findVariableSpecsFromStudyVocabulary <- function(vocabs, variables, getCollectionMemberVarSpecs = c("Dynamic", "Never", "Always")) {
  if (!inherits(vocabs, 'StudySpecificVocabulariesByVariableList')) stop("The first argument must be of the S4 class `StudySpecificVocabulariesByVariableList`.")
  if (!inherits(variables, 'VariableMetadataList')) stop("The second argument must be of the S4 class `VariableMetadataList`.")
  getCollectionMemberVarSpecs <- veupathUtils::matchArg(getCollectionMemberVarSpecs)

  varSpecsWithVocabs <- VariableSpecList(S4Vectors::SimpleList(lapply(as.list(vocabs), getVariableSpec)))

  if (getCollectionMemberVarSpecs != "Never") {
    varMetadataWithVocabs <- findVariableMetadataFromVariableSpec(variables, varSpecsWithVocabs)
    varSpecsWithVocabs <- getVariableSpec(varMetadataWithVocabs, getCollectionMemberVarSpecs)
    if (is.null(varSpecsWithVocabs)) {
      return(NULL)
    }
    varSpecsWithVocabs <- VariableSpecList(S4Vectors::SimpleList(varSpecsWithVocabs))
  }

  return(varSpecsWithVocabs)
}

getVariableColumnNames <- function(variableMetadata) {
  if (!inherits(variableMetadata, 'VariableMetadata')) stop("The specified object must be of the S4 class `VariableMetadata`.")

  colNames <- veupathUtils::getColName(variableMetadata@variableSpec)

  if (variableMetadata@isCollection) {
    colNames <- unlist(lapply(as.list(variableMetadata@members), veupathUtils::getColName))
  }

  return(colNames)
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
  function(object, variables, verbose = c(TRUE, FALSE)) standardGeneric("getDTWithImputedZeroes"),
  signature = c("object", "variables")
)

#' @importFrom digest digest
#' @export
setMethod('getDTWithImputedZeroes', signature = c('Megastudy', 'VariableMetadataList'), function (object, variables, verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)
  veupathUtils::logWithTime("Start imputing zeroes...", verbose)
  
  weightingVariablesMetadata <- findWeightingVariablesMetadata(variables)
  if (is.null(weightingVariablesMetadata))  {
    veupathUtils::logWithTime("No weighting variables present in the plot. No imputation will be done.", verbose)
    return(object@data)
  }

  .dt <- object@data
  veupathUtils::logWithTime(paste0("Imputing zeroes for data.table with ", ncol(.dt), " columns and ", nrow(.dt), " rows"), verbose)
  allEntityIdColumns <- object@ancestorIdColumns
  vocabs <- object@studySpecificVocabularies
  collectionsDT <- object@collectionsDT

  # it seems a lot of this validation could belong to some custom obj w both a megastudy and vm slot.. but what is that? a MegastudyPlot?
  # plus going that route means using this class in plot.data means an api change for plot.data
  # that api change might be worth making in any case, but not doing it now
  ## TODO validate that any collections variables are present in collectionsDT
  variableMetadataNeedingStudyVocabularies <- findStudyDependentVocabularyVariableMetadata(variables)
  variableSpecsWithStudyVocabs <- findVariableSpecsFromStudyVocabulary(vocabs, variables, "Never")
  variableCollectionSpecsWithStudyVocabs <- findVariableSpecsFromStudyVocabulary(vocabs, variables, "Always")
  variableMetadataForStudyVocabVariables <- findVariableMetadataFromVariableSpec(variables, variableSpecsWithStudyVocabs)
  if (length(variableMetadataForStudyVocabVariables) < length(variableMetadataNeedingStudyVocabularies)) {
    stop("Some provided variables require study vocabularies but dont have one.")
  }
  if (length(weightingVariablesMetadata) > 1) {
    stop("Megastudy class does not yet support imputing zeroes when there is more than one weighting variable present.")
  }
  weightingVarSpecsForStudyVocabVariables <- findWeightingVariableSpecs(variableMetadataForStudyVocabVariables)
  if (length(vocabs) > 1) {    
    weightingVarColName <- unique(unlist(lapply(weightingVarSpecsForStudyVocabVariables, veupathUtils::getColName)))
    if (length(weightingVarColName) > 1) {
      stop("All study vocabularies must belong to variables on the same entity using the same weighting variable.")
    }
  } else {
    weightingVarColName <- veupathUtils::getColName(findVariableMetadataFromVariableSpec(variables, veupathUtils::getVariableSpec(vocabs[[1]]))[[1]]@weightingVariableSpec)
  }

  veupathUtils::logWithTime("Finding variables with study vocabularies...", verbose)
  variableSpecsToImputeZeroesFor <- veupathUtils::getVariableSpec(variableMetadataForStudyVocabVariables)
  studyIdColName <- getStudyIdColumnName(vocabs)
  varSpecColNames <- unlist(lapply(variableSpecsToImputeZeroesFor, veupathUtils::getColName))
  # this works bc we validate all vocabs must be on the same entity
  varSpecEntityIdColName <- findEntityIdColumnNameForVariableSpec(veupathUtils::getVariableSpec(vocabs[[1]]), allEntityIdColumns)
  variablesFromEntityOfInterest <- findVariableMetadataFromEntityId(variables, veupathUtils::getVariableSpec(vocabs[[1]])@entityId)
  variableSpecsFromEntityOfInterest <- veupathUtils::getVariableSpec(variablesFromEntityOfInterest)
  if (any(unlist(getHasStudyDependentVocabulary(variablesFromEntityOfInterest)) & 
          unlist(lapply(variableSpecsFromEntityOfInterest, identical, weightingVarSpecsForStudyVocabVariables[[1]])))) {
    stop("Not all variables on the entity associated with the present study vocabulary have study vocabularies.")
  }
  veupathUtils::logWithTime("Imputing zeroes request validated.", verbose)

  # !!!! this assumes entity ids are passed in order, from a single branch
  # alternative would i guess be to make this class aware of the entity diagram
  upstreamEntityIdColNames <- allEntityIdColumns[1:(which(allEntityIdColumns %in% varSpecEntityIdColName)-1)]
  if (!all(allEntityIdColumns[!allEntityIdColumns %in% varSpecEntityIdColName] %in% upstreamEntityIdColNames)) {
    # if we have downstream entities, it doesnt make sense to do all this work. plot.data will just remove the imputed values.
    # if/when the map supports missingness and NA values on downstream entities start to matter, we can revisit.
    veupathUtils::logWithTime("Downstream entities present. No imputation will be done (for now... mwahahaha).", verbose)
    return(.dt)
  }
  studyEntityIdColName <- upstreamEntityIdColNames[1] # still working off the assumption theyre ordered

  # variables that are from the upstream entities need to be in collectionsDT
  # otherwise we erroneously try to impute values for those variables too, rather than only the weighting variable
  upstreamEntities <- veupathUtils::strSplit(upstreamEntityIdColNames, ".", 2, 1)
  if (!!length(collectionsDT)) {
    upstreamEntityVariableColNames <- findColNamesByPredicate(variables, function(x) { x@variableSpec@entityId %in% upstreamEntities })
    if (!all(upstreamEntityVariableColNames %in% names(collectionsDT))) {
      stop("All variables from the upstream entities must be in collectionsDT.")
    }
  }

  # for upstream entities data
  upstreamEntityVariables.dt <- .dt[, -c(weightingVarColName, varSpecColNames), with=FALSE]
  upstreamEntityVariables.dt[[varSpecEntityIdColName]] <- NULL
  upstreamEntityVariables.dt <- unique(upstreamEntityVariables.dt)
  veupathUtils::logWithTime(paste("Found", nrow(upstreamEntityVariables.dt), "unique existing upstream variable value combinations."), verbose)
  if (!!length(collectionsDT)) {
    upstreamEntityVariables.dt <- collectionsDT
  }
  entityIds.dt <- unique(.dt[, c(upstreamEntityIdColNames, varSpecEntityIdColName), with=FALSE])

  # make all possible variable value combinations table
  vocabDTs <- lapply(vocabs, function(x) {x@studyVocab})
  if (!!length(collectionsDT)) {
    vocabDTs <- lapply(vocabDTs, function(x) { merge(x, collectionsDT[, upstreamEntityIdColNames], by=studyEntityIdColName, all=TRUE, allow.cartesian=TRUE) })
  }
  mergeBy <- studyEntityIdColName
  if (!!length(collectionsDT)) mergeBy <- upstreamEntityIdColNames
  allCombinations.dt <- purrr::reduce(vocabDTs, merge, by = mergeBy, allow.cartesian=TRUE, all=TRUE)

  # find which ones we need to add
  presentCombinations.dt <- unique(.dt[, c(upstreamEntityIdColNames, varSpecColNames), with=FALSE])
  # need upstream entity ids for all combinations in order to properly find and merge missing values
  allCombinations.dt <- merge(allCombinations.dt, upstreamEntityVariables.dt, by = mergeBy, all = TRUE, allow.cartesian=TRUE)
  # NOTE: we're assuming if a value was explicitly filtered against that its not in the vocab
  addCombinations.dt <- allCombinations.dt[!presentCombinations.dt, on=c(upstreamEntityIdColNames, varSpecColNames)]

  if (nrow(addCombinations.dt) == 0) {
    veupathUtils::logWithTime("No new combinations to add. Returning existing table.", verbose)
    return(.dt)
  } else {
    veupathUtils::logWithTime(paste("Adding", nrow(addCombinations.dt), "new combinations."), verbose)
  }

  # go ahead and add them, first filling in values for all columns
  addCombinations.dt[[weightingVarColName]] <- 0  
  addCombinations.dt[[varSpecEntityIdColName]] <- stringi::stri_rand_strings(nrow(addCombinations.dt), 10)
  # bind them to the existing rows
  upstreamVariablesInCollectionsDT <- names(collectionsDT)[!names(collectionsDT) %in% upstreamEntityIdColNames]
  if (!!length(collectionsDT) & !all(upstreamVariablesInCollectionsDT %in% names(.dt))) {
    .dt <- merge(.dt, upstreamEntityVariables.dt)
  }
  .dt <- data.table::rbindlist(list(.dt, addCombinations.dt), use.names=TRUE)
  veupathUtils::logWithTime("Added imputed values to existing table. Finished imputing zeroes.", verbose)

  return(.dt)
})
