check_collection_with_metadata <- function(object) {
    errors <- character()
    df <- object@data
    record_id_col <- object@recordIdColumn
    ancestor_id_cols <- object@ancestorIdColumns

    # Ensure id columns are valid
    msg <- validateIdColumns(df, record_id_col, ancestor_id_cols)
    errors <- c(errors, msg)

    # collection data should all come from the same entity
    # using the presence of the period to indicate eda services formatted data
    if (all(grepl(".", names(df), fixed = TRUE))) {
        if (uniqueN(veupathUtils::strSplit(names(df)[!names(df) %in% ancestor_id_cols], ".", ncol=2, index=1)) > 1) {
            msg <- paste("All columns must belong to the same entity.")
            errors <- c(errors, msg)
        }
    }

    allDataColsNumeric <- all(unlist(lapply(df[, !(names(df) %in% c(record_id_col, ancestor_id_cols))], is.numeric)))
    if (inherits(df, 'data.table')) allDataColsNumeric <- all(unlist(lapply(df[, !(names(df) %in% c(record_id_col, ancestor_id_cols)), with=F], is.numeric)))
    if (!allDataColsNumeric) {
        msg <- paste("All columns except the ID columns must be numeric.")
        errors <- c(errors, msg)
    }

    if (!!length(object@sampleMetadata@data)) {
        sampleMetadata <- object@sampleMetadata

        if (!identical(sampleMetadata@recordIdColumn, record_id_col)) {
            msg <- paste("Records in the sample metadata and the collection data refer to different entities.")
        }
        if (!setequal(sampleMetadata@data[[sampleMetadata@recordIdColumn]], df[[record_id_col]])) {
            msg <- paste("Record IDs do not match between the sample metadata and collection data.")
            errors <- c(errors, msg)
        }
        if (!identical(sampleMetadata@data[[sampleMetadata@recordIdColumn]], df[[record_id_col]])) {
            msg <- paste("Records in the sample metadata are not in the same order as in the collection data.")
            errors <- c(errors, msg)
        }
        if (setequal(names(sampleMetadata@data), c(record_id_col, ancestor_id_cols))) {
            msg <- paste("The sample metadata only contains record ID and ancestor ID columns but no metadata variables.")
            errors <- c(errors, msg)
        }
    }

    return(if (length(errors) == 0) TRUE else errors)
}

#' Collection With Metadata
#' 
#' A class for working with numeric data 'collections' and associated metadata.
#' Variable collections should represent a biologically or scientifically coherent concept
#' measured over a consistent or comparable range of values. Examples include relative abundances of various genera,
#' or relative expression levels of genes in a gene expression profile.
#' 
#' @slot data A data.frame of collection values with variables as columns and records as rows
#' @slot sampleMetadata A SampleMetadata object of metadata about the records where records are rows and metadata variables as columns
#' @slot recordIdColumn The name of the column containing IDs for the records. 
#' @slot ancestorIdColumns A character vector of column names representing parent entities of the recordIdColumn.
#' @slot imputeZero A logical indicating whether NA/ null values should be replaced with zeros.
#' @slot removeEmptyRecordss A logical indicating whether empty (all NA/ zero) records should be removed.
#' @name CollectionWithMetadata-class
#' @rdname CollectionWithMetadata-class
#' @include class-SampleMetadata.R
#' @export 
#' @include class-SampleMetadata.R
CollectionWithMetadata <- setClass("CollectionWithMetadata", representation(
    data = 'data.frame',
    sampleMetadata = 'SampleMetadata',
    recordIdColumn = 'character',
    ancestorIdColumns = 'character',
    imputeZero = 'logical',
    removeEmptyRecords = 'logical'
), prototype = prototype(
    recordIdColumn = NA_character_,
    imputeZero = TRUE,
    removeEmptyRecords = TRUE
), validity = check_collection_with_metadata)