check_count_data_collection <- function(object) {
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

    numeric_data <- df[, !(names(df) %in% all_id_cols)]
    if (inherits(df, 'data.table')) numeric_data <- df[, !(names(df) %in% all_id_cols), with=F]

    if (!identical(numeric_data, round(numeric_data))) {
      msg <- "Count data must be integer numbers."
      errors <- c(errors, msg)
    }

    if (any(df < 0, na.rm=TRUE)) {
      msg <- paste("Count data cannot contain negative values.")
      errors <- c(errors, msg)
    }
    

    return(if (length(errors) == 0) TRUE else errors)
}

#' Count Data
#' 
#' A class for working with count data, including microbial or genetic assays.
#' 
#' @slot data A data.frame of integer abundance counts with genes (species, etc.) as columns and samples as rows
#' @slot sampleMetadata A data.frame of metadata about the samples with samples as rows and metadata variables as columns
#' @slot recordIdColumn The name of the column containing IDs for the samples. All other columns will be treated as abundance values.
#' @slot ancestorIdColumns A character vector of column names representing parent entities of the recordIdColumn.
#' @slot imputeZero A logical indicating whether NA/ null values should be replaced with zeros.
#' @name CountDataCollection-class
#' @rdname CountDataCollection-class
#' @include class-CollectionWithMetadata.R
#' @export 
CountDataCollection <- setClass("CountDataCollection", contains = "CollectionWithMetadata", validity = check_count_data_collection)