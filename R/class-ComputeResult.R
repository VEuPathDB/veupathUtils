# so S4 will recognize data.table class as inheriting from data.frame
setOldClass(c("data.table", "data.frame"))

check_compute_result <- function(object) {
    errors <- character()

    if (is.na(object@name)) {
      msg <- "Compute result must have a name."
      errors <- c(errors, msg)
    } else if (length(object@name) != 1) {
      msg <- "Compute result name must have a single value."
      errors <- c(errors, msg) 
    }

    # If computedVariableMetadata is supplied, check that it contains
    # metadata for variables actually in the result data, and that
    # the variable classes are correct.
    if (!!length(object@computedVariableMetadata)) {
      variables <- object@computedVariableMetadata
      col_names <- stripEntityIdFromColumnHeader(veupathUtils::findAllColNames(variables))

      if (!all(col_names %in% names(object@data))) {
        msg <- paste("Some specified computed variables are not present in compute result data.frame")
        errors <- c(errors, msg)
      }

      var_classes <- unlist(lapply(as.list(variables), function(x) {x@variableClass@value}))
      if (!all(var_classes %in% 'computed')) {
        msg <- paste("Some specified computed variables have the wrong variable class.")
        errors <- c(errors, msg) 
      }
    
      # if we have computed variable metadata, we should have data and vice versa
      if (!length(object@data) || nrow(object@data) == 0) {
        msg <- "Compute result must include computed variable metadata or data."
        errors <- c(errors, msg)
      } else {
        if (any(grepl(".", names(object@data), fixed = TRUE))) {
          msg <- paste("Column headers appear to be in dot notation [entityId.variableId]. They should be the raw variableId.")
          errors <- c(errors, msg)
        }
    
        expectedOutputIdColHeaders <- stripEntityIdFromColumnHeader(c(object@recordIdColumn, object@ancestorIdColumns))
        actualOutputIdColHeaders <- names(object@data)[1:length(expectedOutputIdColHeaders)]
        if (all(expectedOutputIdColHeaders != actualOutputIdColHeaders)) {
          msg <- paste("Columns must be ordered by recordIdColumn, ancestorIdColumns, and then data columns.")
          errors <- c(errors, msg) 
        }
      }      
    } else if (!length(object@statistics)) {
      msg <- "Compute result must include computed variable metadata or statistics."
      errors <- c(errors, msg)
    }
   
    return(if (length(errors) == 0) TRUE else errors)
}

#' Compute Result
#' 
#' A class for consistently representing the results of computes. 
#' This includes their representation in R, as JSON and how they are written to files.
#' 
#' @slot data A data.frame of values where computed variables are columns and samples rows.
#' @slot name The name of the compute, ex: 'alphaDiv'.
#' @slot recordIdColumn The name of the column containing IDs for the samples. All other columns will be treated as computed values.
#' @slot ancestorIdColumns A character vector of column names representing parent entities of the recordIdColumn.
#' @slot computedVariableMetadata veupathUtils::VariableMetadataList detailing the computed variables.
#' @slot statistics An optional slot of any values. List or data.frame are recommended. It is not required to have rows or cols map to samples.
#' @slot computationDetails An optional message about the computed results.
#' @slot parameters A record of the input parameters used to generate the computed results.
#' @slot droppedColumns A character vector of column names that have been dropped for being unsuitable for the computation.
#' @name ComputeResult-class
#' @rdname ComputeResult-class
#' @export
#' @include class-VariableMetadata.R
ComputeResult <- setClass("ComputeResult", representation(
    name = 'character',
    data = 'data.frame',
    recordIdColumn = 'character',
    ancestorIdColumns = 'character',
    computedVariableMetadata = 'VariableMetadataList',
    statistics = 'ANY',
    computationDetails = 'character',
    parameters = 'character',
    droppedColumns = 'character'
), prototype = prototype(
    name = NA_character_,
    recordIdColumn = NA_character_,
    computationDetails = NA_character_,
    parameters = NA_character_
), validity = check_compute_result)