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
  return(veupathUtils::getVariableSpec(object[[1]]))
})

#' @export
setMethod('getVariableSpec', signature('ANY'), function(object) {
  return(object@variableSpec)
})

# this might be unexpected behavior. should there be a param to choose between the collection and its member specs?
#' @export
setMethod('getVariableSpec', signature('VariableMetadata'), function(object, getCollectionMemberVarSpecs = c(FALSE, TRUE)) {
  getCollectionMemberVarSpecs <- veupathUtils::matchArg(getCollectionMemberVarSpecs)
  varSpecs <- list(object@variableSpec)

  if (object@isCollection && getCollectionMemberVarSpecs) {
    varSpecs <- as.list(object@members)
  }

  return(varSpecs)
})

#' @export 
setMethod('getVariableSpec', signature('VariableMetadataList'), function(object, getCollectionMemberVarSpecs = c(TRUE, FALSE)) {
  getCollectionMemberVarSpecs <- veupathUtils::matchArg(getCollectionMemberVarSpecs)

  return(unlist(lapply(as.list(object), veupathUtils::getVariableSpec, getCollectionMemberVarSpecs)))
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

findStudyVocabularyByVariableSpec <- function(vocabs, variables, variableSpec) {
  if (!inherits(vocabs, 'StudySpecificVocabulariesByVariableList')) stop("The first argument must be of the S4 class `StudySpecificVocabulariesByVariableList`.")
  if (!inherits(variables, 'VariableMetadataList')) stop("The second argument must be of the S4 class `VariableMetadataList`.")
  if (!inherits(variableSpec, 'VariableSpec')) stop("The third argument must be of the S4 class `VariableSpec`.")

  vocabVariableSpecs <- lapply(as.list(vocabs), veupathUtils::getVariableSpec)
  vocabVariableMetadata <- veupathUtils::findVariableMetadataFromVariableSpec(variables, veupathUtils::VariableSpecList(S4Vectors::SimpleList(vocabVariableSpecs)))
  vocabVariableSpecsAdjustedForVariableCollectionMembers <- veupathUtils::getVariableSpec(vocabVariableMetadata, TRUE)
  
  # if we have found variable collection members in the VariableMetadata, need to check if the passed varspec was a member
  # look through the list that includes the members, and if we match one, get the varspec of the parent/ collection
  # use the varspec of the parent/ collection to get the VariableMetadata associated w the entire collection
  # remember, individual members dont have their own VariableMetadata
  if (!identical(vocabVariableSpecs, vocabVariableSpecsAdjustedForVariableCollectionMembers)) {
    index <- which(purrr::map(vocabVariableSpecsAdjustedForVariableCollectionMembers, function(x) {veupathUtils::getColName(x)}) == veupathUtils::getColName(variableSpec))
    variableCollectionSpecs <- vocabVariableSpecsAdjustedForVariableCollectionMembers[[index]]
    index <- which(purrr::map(vocabVariableMetadata, function(x) {veupathUtils::getColName(variableCollectionSpecs) %in% unlist(veupathUtils::getColName(x@members))}) == TRUE)
  } else {
    index <- which(purrr::map(vocabVariableSpecs, function(x) {veupathUtils::getColName(x)}) == veupathUtils::getColName(variableSpec))
  }

  return(vocabs[[index]])  
}


findVariableSpecsFromStudyVocabulary <- function(vocabs, variables, getCollectionMemberVarSpecs = c(TRUE, FALSE)) {
  if (!inherits(vocabs, 'StudySpecificVocabulariesByVariableList')) stop("The first argument must be of the S4 class `StudySpecificVocabulariesByVariableList`.")
  if (!inherits(variables, 'VariableMetadataList')) stop("The second argument must be of the S4 class `VariableMetadataList`.")
  getCollectionMemberVarSpecs <- veupathUtils::matchArg(getCollectionMemberVarSpecs)

  varSpecsWithVocabs <- VariableSpecList(S4Vectors::SimpleList(lapply(as.list(vocabs), getVariableSpec)))

  if (getCollectionMemberVarSpecs) {
    varMetadataWithVocabs <- findVariableMetadataFromVariableSpec(variables, varSpecsWithVocabs)
    varSpecsWithVocabs <- VariableSpecList(S4Vectors::SimpleList(getVariableSpec(varMetadataWithVocabs, getCollectionMemberVarSpecs)))
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
  message("dt has ", ncol(.dt), " columns and ", nrow(.dt), " rows")
  # TODO feel like im doing this operation a lot.. maybe another method/ helper?
  # also, try to figure a way we dont have to do this.. i dont remember why i did this and its inconsistent behavior
  variableColumnNames <- unlist(lapply(as.list(variables), getVariableColumnNames))
  allEntityIdColumns <- object@ancestorIdColumns
  # drop things that arent in the plot, except ids
  .dt <- .dt[, c(variableColumnNames, allEntityIdColumns), with=FALSE]
  message("dt has ", ncol(.dt), " columns and ", nrow(.dt), " rows after limiting to columns of interest")
  message("head of dt: ", head(.dt,1))
  vocabs <- object@studySpecificVocabularies

  # it seems a lot of this validation could belong to some custom obj w both a megastudy and vm slot.. but what is that? a MegastudyPlot?
  # plus going that route means using this class in plot.data means an api change for plot.data
  # that api change might be worth making in any case, but not doing it now
  variableMetadataNeedingStudyVocabularies <- findStudyDependentVocabularyVariableMetadata(variables)
  variableSpecsWithStudyVocabs <- findVariableSpecsFromStudyVocabulary(vocabs, variables, TRUE)
  variableCollectionSpecsWithStudyVocabs <- findVariableSpecsFromStudyVocabulary(vocabs, variables, FALSE)
  if (is.null(variableCollectionSpecsWithStudyVocabs)) {
    variableMetadataForStudyVocabVariables <- findVariableMetadataFromVariableSpec(variables, variableSpecsWithStudyVocabs)
  } else {
    variableMetadataForStudyVocabVariables <- findVariableMetadataFromVariableSpec(variables, variableCollectionSpecsWithStudyVocabs)
  }
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

  # for upstream entities data
  combinations.dt <- unique(.dt[, -c(weightingVarColName, varSpecColNames), with=FALSE])
  combinations.dt[[varSpecEntityIdColName]] <- NULL
  combinations.dt <- unique(combinations.dt)
  message(paste("Found", nrow(combinations.dt), "possible variable value combinations."))
  message("head(combinations.dt): ", head(combinations.dt, 1))
  entityIds.dt <- unique(.dt[, c(upstreamEntityIdColNames, varSpecEntityIdColName), with=FALSE])
  message("head(entityIds.dt): ", head(entityIds.dt, 1))
  veupathUtils::logWithTime("Found all possible variable value combinations.", verbose)

  # impute zeroes for each study vocab iteratively
  makeImputedZeroesDT <- function(variableSpec) {
    veupathUtils::logWithTime(paste("Imputing zeroes for", veupathUtils::getColName(variableSpec)), verbose)
    varSpecColName <- veupathUtils::getColName(variableSpec)
    vocab <- findStudyVocabularyByVariableSpec(vocabs, variables, variableSpec)
    message("vocab: ", vocab)
    vocabs.dt <- veupathUtils::as.data.table(vocab)
    names(vocabs.dt)[2] <- varSpecColName
    message("cols names: ", colnames(vocabs.dt))
    message("head(vocabs.dt): ", head(vocabs.dt, 1))
    vocabs.dt <- merge(entityIds.dt, vocabs.dt, by=studyIdColName, allow.cartesian=TRUE)
    message("after merge- cols names: ", colnames(vocabs.dt))
    message("after merge- head(vocabs.dt): ", head(vocabs.dt, 1))
    present.dt <- unique(.dt[, c(upstreamEntityIdColNames, varSpecColName), with=FALSE])
    message("cols names: ", colnames(present.dt))
    message("head(present.dt): ", head(present.dt, 1))
    # assume if a value was explicitly filtered against that its not in the vocab
    add.dt <- vocabs.dt[!present.dt, on=c(upstreamEntityIdColNames, varSpecColName)]
    if (nrow(add.dt) > 0) {
      add.dt[[weightingVarColName]] <- 0
    } else {
      add.dt[[weightingVarColName]] <- numeric()
    }
   
    message("cols names: ", colnames(add.dt))
    message("head(add.dt): ", head(add.dt, 1))
    return(unique(add.dt))
  }
  dataTablesOfImputedValues <- lapply(variableSpecsToImputeZeroesFor, makeImputedZeroesDT)
  mergeDTsOfImputedValues <- function(x,y) {
    merge(x, y, by = c(upstreamEntityIdColNames, varSpecEntityIdColName, weightingVarColName), allow.cartesian=TRUE)
  }
  .dt2 <- purrr::reduce(dataTablesOfImputedValues, mergeDTsOfImputedValues)
  message("head(.dt2): ", head(.dt2, 1))
  veupathUtils::logWithTime("Finished collapsing imputed values for all variables into one table.", verbose)
  #make impossibly unique ids
  .dt2[[varSpecEntityIdColName]] <- apply(.dt2[, c(upstreamEntityIdColNames, varSpecColNames), with=FALSE], 1, digest::digest, algo='md5')
  .dt2 <- unique(merge(.dt2, combinations.dt, by=upstreamEntityIdColNames))
  message("with ids- head(.dt2): ", head(.dt2, 1))
  .dt <- rbind(.dt, .dt2)
  veupathUtils::logWithTime("Added imputed values to table. Finished imputing zeroes.", verbose)
  message("head(.dt): ", head(.dt, 1))
  return(.dt)
})
