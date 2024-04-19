#' Manage Collection Names
#' 
#' Get or set the name of the collection
#' @param x A Collection
#' @rdname name
#' @export
setGeneric("name", function(x) standardGeneric("name"))

#' @rdname name
#' @aliases name,Collection-method
setMethod("name", "Collection", function(x) return(x@name))

#' @rdname name
#' @param value The new name of the collection
#' @export
setGeneric("name<-", function(x, value) standardGeneric("name<-"))

#' @rdname name
#' @aliases name<-,Collection,character-method
setMethod("name<-", "Collection", function(x, value) {x@name <- value; return(x)})

#' Get Names of Collections
#' 
#' Get the names of the collections in the Collections or MbioDataset object
#' @param object A Collections object, or MbioDataset
#' @return A character vector of collection names
#' @export
#' @rdname getCollectionNames
setGeneric("getCollectionNames", function(object) standardGeneric("getCollectionNames"))

#' @rdname getCollectionNames
#' @aliases getCollectionNames,Collections-method
setMethod("getCollectionNames", "Collections", function(object) return(sapply(object, name)))

#' Get Collection Variable Names
#' 
#' Get the names of the variables in the Collection
#' @param object A Collection
#' @return A character vector of variable names
#' @export
#' @rdname getCollectionVariableNames
setGeneric("getCollectionVariableNames", function(object) standardGeneric("getCollectionVariableNames"))

#' @rdname getCollectionVariableNames
#' @aliases getCollectionVariableNames,Collection-method
setMethod("getCollectionVariableNames", "Collection", function(object) {
    allIdColumns <- getIdColumns(object)

    return(colnames(object@data)[!(colnames(object@data) %in% allIdColumns)])
})

#' Get data.table of values from Collection
#'
#' Returns a data.table of collection values, respecting the
#' `imputeZero` slot.
#' 
#' @param object Collection
#' @param variableNames A character vector representing the name of the variables to return.
#' If NULL, returns all variables
#' @param ignoreImputeZero boolean indicating whether we should respect the imputeZero slot
#' @param includeIds boolean indicating whether we should include recordIdColumn and ancestorIdColumns
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table of values
#' @rdname getCollectionData
#' @export
setGeneric("getCollectionData",
    function(
        object, 
        variableNames = NULL,
        ignoreImputeZero = c(FALSE, TRUE), 
        includeIds = c(TRUE, FALSE), 
        verbose = c(TRUE, FALSE)
    ) standardGeneric("getCollectionData"),
    signature = c("object")
)

#' @rdname getCollectionData
#' @aliases getCollectionData,Collection-method
setMethod("getCollectionData", signature("Collection"), 
function(
    object, 
    variableNames = NULL,
    ignoreImputeZero = c(FALSE, TRUE), 
    includeIds = c(TRUE, FALSE), 
    verbose = c(TRUE, FALSE)
) {
    ignoreImputeZero <- veupathUtils::matchArg(ignoreImputeZero)
    includeIds <- veupathUtils::matchArg(includeIds)
    verbose <- veupathUtils::matchArg(verbose)

    if (is.null(variableNames)) {
        dt <- object@data
    } else {
        dt <- object@data[, variableNames, with = FALSE]
    }
    allIdColumns <- getIdColumns(object)

    # Check that incoming dt meets requirements
    if (!inherits(dt, 'data.table')) {
        # this might technically be bad form, but i think its ok in this context
        data.table::setDT(dt)
    }

    if (object@removeEmptyRecords) {
        dt.noIds <- dt[, -..allIdColumns]
        # Remove records with NA or 0 in all columns
        dt <- dt[rowSums(isNAorZero(dt.noIds)) != ncol(dt.noIds),]
        numRecordsRemoved <- nrow(dt.noIds) - nrow(dt)
        if (numRecordsRemoved > 0) {
            veupathUtils::logWithTime(paste0("Removed ", numRecordsRemoved, " records with no data."), verbose)
        }
    }

    # Replace NA values with 0
    if (!ignoreImputeZero && object@imputeZero) {
        veupathUtils::setNaToZero(dt)
    }

    if (!includeIds) {
        dt <- dt[, -..allIdColumns]
    }

    return(dt)
})

#' Prune features by predicate
#' 
#' Modifies the data slot of a Collection or
#' CollectionWithMetadata object, to exclude features for which 
#' the provided predicate function returns FALSE. 
#' 
#' @param object Collection
#' @param predicate Function returning a boolean indicating if a feature should be included (TRUE) or excluded (FALSE)
#' @param verbose boolean indicating if timed logging is desired
#' @return Collection with modified data slot
#' @rdname pruneFeatures
#' @export
setGeneric("pruneFeatures",
    function(object, predicate, verbose = c(TRUE, FALSE)) standardGeneric("pruneFeatures"),
    signature = c("object")
)

#' @rdname pruneFeatures
#' @aliases pruneFeatures,Collection-method 
setMethod("pruneFeatures", signature("Collection"), function(object, predicate, verbose = c(TRUE, FALSE)) {
    df <- getCollectionData(object)
    allIdColumns <- c(object@recordIdColumn, object@ancestorIdColumns)

    # keep columns that pass the predicate, while respecting removeEmptyRecords
    keepCols <- df[, lapply(.SD, predicate), .SDcols = colnames(df)[!(colnames(df) %in% allIdColumns)]]
    keepCols <- names(keepCols)[keepCols == TRUE]
    # df used to find keepCols, but we dont want to modify the rows as well, so we stop using df now
    # bc df was made by calling getCollectionData which currently pretty religiously respects removeEmptyRecords
    object@data <- object@data[, c(allIdColumns, keepCols), with = FALSE]

    validObject(object)
    return(object)
})