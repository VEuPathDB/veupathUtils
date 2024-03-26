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

#' Get data.table of values from Collection
#'
#' Returns a data.table of collection values, respecting the
#' `imputeZero` slot.
#' 
#' @param object Collection
#' @param ignoreImputeZero boolean indicating whether we should respect the imputeZero slot
#' @param includeIds boolean indicating whether we should include recordIdColumn and ancestorIdColumns
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table of values
#' @rdname getCollectionData
#' @export
setGeneric("getCollectionData",
    function(object, ignoreImputeZero = c(FALSE, TRUE), includeIds = c(TRUE, FALSE), verbose = c(TRUE, FALSE)) standardGeneric("getCollectionData"),
    signature = c("object")
)

#' @rdname getCollectionData
#' @aliases getCollectionData,Collection-method
setMethod("getCollectionData", signature("Collection"), function(object, ignoreImputeZero = c(FALSE, TRUE), includeIds = c(TRUE, FALSE), verbose = c(TRUE, FALSE)) {
    ignoreImputeZero <- veupathUtils::matchArg(ignoreImputeZero)
    includeIds <- veupathUtils::matchArg(includeIds)
    verbose <- veupathUtils::matchArg(verbose)

    dt <- object@data
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