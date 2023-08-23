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
setMethod('getVariableSpec', signature('StudySpecificVocabulariesByVariable'), function(object) {
  return(veupathUtils::getVariableSpec(object[[1]]))
})

#' @export
setMethod('getVariableSpec', signature('ANY'), function(object) {
  return(object@variableSpec)
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
  #works bc of validation
  return(veupathUtils::getStudyIdColumnName(object[[1]]))
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

#' @include methods-Statistic.R
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
findEntityIdColumnNameForVariableSpec <- function(varSpec, entityIdColumns) {
  if (!inherits(varSpec, 'VariableSpec')) stop("The first argument must be of the S4 class `VariableSpec`.")

  return(entityIdColumns[grepl(varSpec@entityId, entityIdColumns)])
}

findStudyVocabularyByVariableSpec <- function(vocabs, variableSpec) {
  if (!inherits(vocabs, 'StudySpecificVocabulariesByVariableList')) stop("The first argument must be of the S4 class `StudySpecificVocabulariesByVariableList`.")
  if (!inherits(variableSpec, 'VariableSpec')) stop("The second argument must be of the S4 class `VariableSpec`.")

  vocabVariableSpecs <- lapply(as.list(vocabs), veupathUtils::getVariableSpec)
  index <- which(purrr::map(vocabVariableSpecs, function(x) {veupathUtils::getColName(x)}) == veupathUtils::getColName(variableSpec))

  return(vocabs[[index]])  
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
  # TODO feel like im doing this operation a lot.. maybe another method/ helper?
  # also, try to figure a way we dont have to do this.. i dont remember why i did this and its inconsistent behavior
  variableColumnNames <- unlist(lapply(lapply(as.list(variables), veupathUtils::getVariableSpec), veupathUtils::getColName))
  allEntityIdColumns <- object@ancestorIdColumns
  # drop things that arent in the plot, except ids
  .dt <- .dt[, c(variableColumnNames, allEntityIdColumns), with=FALSE]
  vocabs <- object@studySpecificVocabularies

  # it seems a lot of this validation could belong to some custom obj w both a megastudy and vm slot.. but what is that? a MegastudyPlot?
  # plus going that route means using this class in plot.data means an api change for plot.data
  # that api change might be worth making in any case, but not doing it now
  variableMetadataNeedingStudyVocabularies <- findStudyDependentVocabularyVariableMetadata(variables)
  variableSpecsWithStudyVocabs <- VariableSpecList(S4Vectors::SimpleList(lapply(as.list(vocabs), getVariableSpec)))
  variableMetadataForStudyVocabVariables <- findVariableMetadataFromVariableSpec(variables, variableSpecsWithStudyVocabs)
  if (length(variableSpecsWithStudyVocabs) > length(variableMetadataForStudyVocabVariables)) {
    warning("Study vocabularies were provided for variables that are not present in the plot. These will be ignored.")
  }
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

  studyIdColName <- getStudyIdColumnName(vocabs)
  # TODO should this be made based on variableSpecsTiImputeZeroesFor ??
  varSpecColNames <- getVariableSpecColumnName(vocabs)
  # this works bc we validate all vocabs must be on the same entity
  varSpecEntityIdColName <- findEntityIdColumnNameForVariableSpec(veupathUtils::getVariableSpec(vocabs[[1]]), allEntityIdColumns)
  variablesFromEntityOfInterest <- findVariableMetadataFromEntityId(variables, veupathUtils::getVariableSpec(vocabs[[1]])@entityId)
  variableSpecsFromEntityOfInterest <- lapply(as.list(variablesFromEntityOfInterest), getVariableSpec)
  if (any(unlist(getHasStudyDependentVocabulary(variablesFromEntityOfInterest)) & 
          unlist(lapply(variableSpecsFromEntityOfInterest, identical, weightingVarSpecsForStudyVocabVariables[[1]])))) {
    stop("Not all variables on the entity associated with the present study vocabulary have study vocabularies.")
  }
  # !!!! this assumes entity ids are passed in order, from a single branch
  # alternative would i guess be to make this class aware of the entity diagram
  upstreamEntityIdColNames <- allEntityIdColumns[1:(which(allEntityIdColumns %in% varSpecEntityIdColName)-1)]
  if (!all(allEntityIdColumns[!allEntityIdColumns %in% varSpecEntityIdColName] %in% upstreamEntityIdColNames)) {
    # if we have downstream entities, it doesnt make sense to do all this work. plot.data will just remove the imputed values.
    # if/when the map supports missingness and NA values on downstream entities start to matter, we can revisit.
    return(.dt)
  }

  # for upstream entities data
  combinations.dt <- unique(.dt[, -c(weightingVarColName, varSpecColNames), with=FALSE])
  combinations.dt[[varSpecEntityIdColName]] <- NULL
  combinations.dt <- unique(combinations.dt)
  entityIds.dt <- unique(.dt[, c(upstreamEntityIdColNames, varSpecEntityIdColName), with=FALSE])

  # impute zeroes for each study vocab iteratively
  variableSpecsToImputeZeroesFor <- lapply(as.list(variableMetadataForStudyVocabVariables), veupathUtils::getVariableSpec)
  makeImputedZeroesDT <- function(variableSpec) {
    vocab <- findStudyVocabularyByVariableSpec(vocabs, variableSpec)
    vocabs.dt <- merge(entityIds.dt, veupathUtils::as.data.table(vocab), by=studyIdColName, allow.cartesian=TRUE)
    varSpecColName <- veupathUtils::getColName(variableSpec)
    present.dt <- unique(.dt[, c(upstreamEntityIdColNames, varSpecColName), with=FALSE])
    # assume if a value was explicitly filtered against that its not in the vocab
    add.dt <- vocabs.dt[!present.dt, on=c(upstreamEntityIdColNames, varSpecColName)]
    if (nrow(add.dt) > 0) {
      add.dt[[weightingVarColName]] <- 0
    } else {
      add.dt[[weightingVarColName]] <- numeric()
    }
   
    return(unique(add.dt))
  }
  dataTablesOfImputedValues <- lapply(variableSpecsToImputeZeroesFor, makeImputedZeroesDT)
  mergeDTsOfImputedValues <- function(x,y) {
    # TODO test this merge, not sure i got it right..
    merge(x, y, by = c(upstreamEntityIdColNames, varSpecEntityIdColName, weightingVarColName), allow.cartesian=TRUE)
  }
  .dt2 <- purrr::reduce(dataTablesOfImputedValues, mergeDTsOfImputedValues)
  #make impossibly unique ids
  .dt2[[varSpecEntityIdColName]] <- apply(.dt2[, c(upstreamEntityIdColNames, varSpecColNames), with=FALSE], 1, digest::digest, algo='md5')
  .dt2 <- unique(merge(.dt2, combinations.dt, by=upstreamEntityIdColNames))
  
  .dt <- rbind(.dt, .dt2)

  return(.dt)
})