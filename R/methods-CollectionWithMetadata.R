#' Get all ID columns
#' 
#' Returns a vector of all ID columns
#' 
#' @param object CollectionWithMetadata, or other object with slots recordIdColumn and ancestorIdColumns
#' @return vector of all ID columns
#' @rdname getIdColumns
#' @export
#' @include class-CollectionWithMetadata.R
setGeneric("getIdColumns",
    function(object) standardGeneric("getIdColumns"),
    signature = c("object")
)

#' @rdname getIdColumns
#' @aliases getIdColumns,ANY-method
setMethod("getIdColumns", "ANY", function(object) {
    if (all(c('recordIdColumn','ancestorIdColumns') %in% slotNames(object))) {
        return(c(object@recordIdColumn, object@ancestorIdColumns))
    } else {
        stop("Object does not have recordIdColumn and/or ancestorIdColumns slots. Received object of class ", class(object))
    }
})

#' Get Variable Names of Metadata
#' 
#' Get the names of the metadata variables in an object containing sample metadata.
#' @param object Any object with a slot containing sampleMetadata
#' @return a character vector of metadata variable names
#' @rdname getMetadataVariableNames
#' @export
setGeneric("getMetadataVariableNames", function(object) standardGeneric("getMetadataVariableNames"))

#' @rdname getMetadataVariableNames
#' @aliases getMetadataVariableNames,CollectionWithMetadata-method
setMethod("getMetadataVariableNames", "CollectionWithMetadata", function(object) return(names(object@sampleMetadata@data)))

#' Get Sample Metadata Id Column Names
#' 
#' Get the names of the record and ancestor id columns in the sample metadata of the Microbiome Dataset.
#' @param object A Microbiome Dataset, or other object w sample metadata
#' @return a character vector of id column names
#' @rdname getSampleMetadataIdColumns
#' @export
setGeneric("getSampleMetadataIdColumns", function(object) standardGeneric("getSampleMetadataIdColumns"))

#' @rdname getSampleMetadataIdColumns
#' @aliases getSampleMetadataIdColumns,CollectionWithMetadata-method
setMethod("getSampleMetadataIdColumns", "CollectionWithMetadata", function(object) getIdColumns(object@sampleMetadata))

#' Get data.table of values from CollectionWithMetadata
#'
#' Returns a data.table of collection values, respecting the
#' `imputeZero` slot.
#' 
#' @param object CollectionWithMetadata
#' @param ignoreImputeZero boolean indicating whether we should respect the imputeZero slot
#' @param includeIds boolean indicating whether we should include recordIdColumn and ancestorIdColumns
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table of values
#' @rdname getCollectionValues
#' @export
setGeneric("getCollectionValues",
    function(object, ignoreImputeZero = c(FALSE, TRUE), includeIds = c(TRUE, FALSE), verbose = c(TRUE, FALSE)) standardGeneric("getCollectionValues"),
    signature = c("object")
)

#' @rdname getCollectionValues
#' @aliases getCollectionValues,CollectionWithMetadata-method
setMethod("getCollectionValues", signature("CollectionWithMetadata"), function(object, ignoreImputeZero = c(FALSE, TRUE), includeIds = c(TRUE, FALSE), verbose = c(TRUE, FALSE)) {
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

#' Get data.table of sample metadata from CollectionWithMetadata
#'
#' Returns a data.table of sample metadata
#' 
#' @param object CollectionWithMetadata
#' @param asCopy boolean indicating whether to return the data as a copy or by reference
#' @param includeIds boolean indicating whether we should include recordIdColumn and ancestorIdColumns
#' @param metadataVariables The metadata variables to include in the sample metadata. If NULL, all metadata variables will be included.
#' @return data.table of sample metadata
#' @import veupathUtils
#' @import data.table
#' @rdname getSampleMetadata
#' @export
setGeneric("getSampleMetadata",
    function(object, asCopy = c(TRUE, FALSE), includeIds = c(TRUE, FALSE), metadataVariables = NULL) standardGeneric("getSampleMetadata"),
    signature = c("object")
)

#' @rdname getSampleMetadata
#' @aliases getSampleMetadata,CollectionWithMetadata-method
setMethod("getSampleMetadata", signature("CollectionWithMetadata"), function(object, asCopy = c(TRUE, FALSE), includeIds = c(TRUE, FALSE), metadataVariables = NULL) {
    asCopy <- veupathUtils::matchArg(asCopy)
    includeIds <- veupathUtils::matchArg(includeIds)

    dt <- object@sampleMetadata@data
    allIdColumns <- getSampleMetadataIdColumns(object)

    # Check that incoming dt meets requirements
    if (!inherits(dt, 'data.table')) {
        data.table::setDT(dt)
    }

    if (asCopy) {
        dt <- data.table::copy(dt)
    }

    if (object@removeEmptyRecords) {
        # not using getCollectionValues here bc i want the empty records here
        abundances <- object@data[, -..allIdColumns]

        # Remove metadata for records with NA or 0 in all columns
        dt <- dt[rowSums(isNAorZero(abundances)) != ncol(abundances),]
    }

    if (includeIds && !is.null(metadataVariables)) {
        dt <- dt[, c(allIdColumns, metadataVariables), with = FALSE]
    } else if (!includeIds && !is.null(metadataVariables)) {
        dt <- dt[, metadataVariables, with = FALSE]
    } else if (!includeIds && is.null(metadataVariables)) {
        dt <- dt[, -..allIdColumns]
    }

    return(dt)
})

#' Drop Records with incomplete SampleMetadata
#'
#' Modifies the data and sampleMetadata slots of an 
#' CollectionWithMetadata object, to exclude records with 
#' missing SampleMetadata for a specified column.
#' 
#' @param object CollectionWithMetadata
#' @param colName String providing the column name in SampleMetadata to check for completeness
#' @param verbose boolean indicating if timed logging is desired
#' @return CollectionWithMetadata with modified data and sampleMetadata slots
#' @rdname removeIncompleteRecords
#' @export
setGeneric("removeIncompleteRecords",
    function(object, colName = character(), verbose = c(TRUE, FALSE)) standardGeneric("removeIncompleteRecords"),
    signature = c("object")
)

#' @rdname removeIncompleteRecords
#' @aliases removeIncompleteRecords,CollectionWithMetadata-method
setMethod("removeIncompleteRecords", signature("CollectionWithMetadata"), function(object, colName = character(), verbose = c(TRUE, FALSE)) {
    verbose <- veupathUtils::matchArg(verbose)

    df <- getCollectionValues(object, verbose = verbose)
    sampleMetadata <- getSampleMetadata(object)

    # Remove Records with NA from data and metadata
    if (any(is.na(sampleMetadata[[colName]]))) {
        veupathUtils::logWithTime("Found NAs in specified variable. Removing these records.", verbose)
        recordsWithData <- which(!is.na(sampleMetadata[[colName]]))
        # Keep records with data. Recall the CollectionWithMetadata object requires records to be in the same order
        # in both the data and metadata
        sampleMetadata <- sampleMetadata[recordsWithData, ]
        df <- df[recordsWithData, ]

        object@data <- df
        object@sampleMetadata <- SampleMetadata(
            data = sampleMetadata,
            recordIdColumn = object@sampleMetadata@recordIdColumn
        )
        validObject(object)
    }

    return(object)
})

#' Prune features by predicate
#' 
#' Modifies the data slot of an 
#' CollectionWithMetadata object, to exclude features for which 
#' the provided predicate function returns FALSE.
#' 
#' @param object CollectionWithMetadata
#' @param predicate Function returning a boolean indicating if a feature should be included (TRUE) or excluded (FALSE)
#' @param verbose boolean indicating if timed logging is desired
#' @return CollectionWithMetadata with modified data slot
#' @rdname pruneFeatures
#' @export
setGeneric("pruneFeatures",
    function(object, predicate, verbose = c(TRUE, FALSE)) standardGeneric("pruneFeatures"),
    signature = c("object")
)

#' @rdname pruneFeatures
#' @aliases pruneFeatures,CollectionWithMetadata-method 
setMethod("pruneFeatures", signature("CollectionWithMetadata"), function(object, predicate, verbose = c(TRUE, FALSE)) {
    df <- getCollectionValues(object)
    allIdColumns <- c(object@recordIdColumn, object@ancestorIdColumns)

    # keep columns that pass the predicate
    keepCols <- df[, lapply(.SD, predicate), .SDcols = colnames(df)[!(colnames(df) %in% allIdColumns)]]
    keepCols <- names(keepCols)[keepCols == TRUE]
    df <- df[, c(allIdColumns, keepCols), with = FALSE]

    # LET ME EXPLAIN
    # since we called getCollectionValues (which removes empty records)..
    # we need to do the same for sampleMetadata in order to produce a valid object.
    # and, we dont want those empty records influencing which features get pruned, so i think were tied to this.
    # we just need to be sure we ask for the metadata before resetting the abundance data, or else we'll get an error
    # bc getSampleMetadata also calls getCollectionValues to find which records to remove
    # we could maybe do it better, by introducing a hasSampleMetadata method in here. but i'm not sure if that's worth it.
    if (nrow(object@sampleMetadata@data) > 0) {
        object@sampleMetadata@data <- getSampleMetadata(object)
    }
    object@data <- df
    validObject(object)
    return(object)
})