check_array_data_collection <- function(object) {
    errors <- character()
    df <- object@data
    record_id_col <- object@recordIdColumn
    ancestor_id_cols <- object@ancestorIdColumns
    all_id_cols <- c(record_id_col, ancestor_id_cols)


    allDataColsNumeric <- all(unlist(lapply(df[, !(names(df) %in% c(record_id_col, ancestor_id_cols))], is.numeric)))
    if (inherits(df, 'data.table')) allDataColsNumeric <- all(unlist(lapply(df[, !(names(df) %in% c(record_id_col, ancestor_id_cols)), with=F], is.numeric)))
    if (!allDataColsNumeric) {
      msg <- paste("All columns except the ID columns must be numeric.")
      errors <- c(errors, msg)
    }

    # Unlike CountDataCollection, we do NOT check for integers
    # Unlike CountDataCollection, we do NOT check for non-negative values
    # Array data can be negative (log-transformed, normalized, etc.)


    return(if (length(errors) == 0) TRUE else errors)
}

#' Array Data Collection
#'
#' A class for working with continuous expression data from arrays (antibody arrays,
#' gene expression microarrays, etc.). Unlike CountDataCollection, this class accepts
#' continuous numeric data including negative values (e.g., log-transformed data).
#'
#' Users are expected to provide pre-normalized data. No normalization is performed
#' by veupathUtils methods.
#'
#' @slot data A data.frame of numeric expression values with features as columns and samples as rows
#' @slot sampleMetadata A data.frame of metadata about samples with samples as rows and metadata variables as columns
#' @slot recordIdColumn The name of the column containing IDs for the samples
#' @slot ancestorIdColumns A character vector of column names representing parent entities of the recordIdColumn
#' @slot imputeZero A logical indicating whether NA/null values should be replaced with zeros
#' @name ArrayDataCollection-class
#' @rdname ArrayDataCollection-class
#' @include class-CollectionWithMetadata.R
#' @export
ArrayDataCollection <- setClass("ArrayDataCollection",
    contains = "CollectionWithMetadata",
    validity = check_array_data_collection)
