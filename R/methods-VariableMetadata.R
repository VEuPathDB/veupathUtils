#' R object as JSON string
#' 
#' This function converts an R object to a JSON string.
#' see `methods(veupathUtils::toJSON)` for a list of support classes.
#' @param object object of a supported S4 class to convert to a JSON string representation
#' @param named logical indicating whether the result should be a complete named JSON object or just the value of the object
#' @return character vector of length 1 containing JSON string
#' @export
setGeneric("toJSON", 
  function(object, named = c(TRUE, FALSE)) standardGeneric("toJSON"),
  signature = "object"
)

#' @export
setMethod("toJSON", signature("VariableClass"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- jsonlite::toJSON(object@value)

    if (named) tmp <- paste0('{"variableClass":', tmp, '}')
    
    return(tmp)
})

#' @export
setMethod("toJSON", signature("VariableSpec"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- list("variableId" = jsonlite::unbox(object@variableId),
                "entityId" = jsonlite::unbox(object@entityId))

    if (named) tmp <- list("variableSpec" = tmp)

    return(jsonlite::toJSON(tmp))
})

#' @export
setMethod("toJSON", signature("PlotReference"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- jsonlite::toJSON(object@value)

    if (named) tmp <- paste0('{"plotReference":', tmp, '}')
    
    return(tmp)
})

#' @export
setMethod("toJSON", signature("VariableSpecList"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- S4SimpleListToJSON(object)

    if (named) tmp <- paste0('{"variableSpecs":', tmp, "}")

    return(tmp)
})

#' @export
setMethod("toJSON", signature("DataType"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- jsonlite::unbox(object@value)

    if (named) tmp <- list("dataType" = tmp)
    
    return(jsonlite::toJSON(tmp))
})

#' @export
setMethod("toJSON", signature("DataShape"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- jsonlite::unbox(object@value)

    if (named) tmp <- list("dataShape" = tmp)
    
    return(jsonlite::toJSON(tmp))
})

#' @export
setMethod("toJSON", signature("VariableMetadata"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named)    
    tmp <- character()

    variable_class_json <- veupathUtils::toJSON(object@variableClass, named = FALSE)
    variable_spec_json <- veupathUtils::toJSON(object@variableSpec, named = FALSE)
    plot_reference_json <- veupathUtils::toJSON(object@plotReference, named = FALSE)

    tmp <- paste0('"variableClass":', variable_class_json, ',"variableSpec":', variable_spec_json, ',"plotReference":', plot_reference_json)

    if (!is.na(object@displayName)) {
      display_name_json <- jsonlite::toJSON(jsonlite::unbox(object@displayName))
      tmp <- paste0(tmp, ',"displayName":', display_name_json)
    }

    if (!is.na(object@displayRangeMin)) {
      display_range_min_json <- jsonlite::toJSON(jsonlite::unbox(as.character(object@displayRangeMin)))
      tmp <- paste0(tmp, ',"displayRangeMin":', display_range_min_json)
    }

    if (!is.na(object@displayRangeMax)) {
      display_range_max_json <- jsonlite::toJSON(jsonlite::unbox(as.character(object@displayRangeMax)))
      tmp <- paste0(tmp, ',"displayRangeMax":', display_range_max_json)
    }

    if (!is.na(object@dataType@value)) {
      data_type_json <- veupathUtils::toJSON(object@dataType, named = FALSE)
      tmp <- paste0(tmp, ',"dataType":', data_type_json)
    }
    
    if (!is.na(object@dataType@value)) {
      data_shape_json <- veupathUtils::toJSON(object@dataShape, named = FALSE)
      tmp <- paste0(tmp, ',"dataShape":', data_shape_json)
    }

    if (!all(is.na(object@vocabulary))) {
      vocabulary_json <- jsonlite::toJSON(object@vocabulary)
      tmp <- paste0(tmp, ',"vocabulary":', vocabulary_json)
    }

    tmp <- paste0(tmp, ',"isCollection":', jsonlite::toJSON(jsonlite::unbox(object@isCollection)))
    tmp <- paste0(tmp, ',"imputeZero":', jsonlite::toJSON(jsonlite::unbox(object@imputeZero)))
    
    if (!!length(object@members)) {
      members_json <- veupathUtils::toJSON(object@members, named = FALSE)
      tmp <- paste0(tmp, ',"members":', members_json)
    }
    
    tmp <- paste0("{", tmp, "}")
    if (named) tmp <- paste0('{"variableMetadata":', tmp, '}')

    return(tmp)
})

#' @export
setMethod("toJSON", signature("VariableMetadataList"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- S4SimpleListToJSON(object)

    if (named) tmp <- paste0('{"variables":', tmp, "}")

    return(tmp)
})

#' EDA Variable Column Names matching a PlotReference
#' 
#' This function provides EDA-compliant column names provided
#' an EDA-compliant VariableMetadataList object.
#' @param variables a VariableMetadataList of variables to search
#' @param plotRef a string representing the PlotReference to look for
#' @return character vector of column names matching the provided plot reference
#' @export
setGeneric("findColNamesFromPlotRef", 
  function(variables, plotRef) standardGeneric("findColNamesFromPlotRef"),
  signature = "variables"
)

#' @export
setMethod("findColNamesFromPlotRef", signature("VariableMetadataList"), function(variables, plotRef) {
  colNames <- veupathUtils::findColNamesByPredicate(variables, function(x) {if (x@plotReference@value == plotRef) TRUE})

  return(colNames)
})

#' EDA Variable Data Types matching a PlotReference
#' 
#' This function provides EDA-compliant data types provided
#' an EDA-compliant VariableMetadataList object.
#' @param variables a VariableMetadataList of variables to search
#' @param plotRef a string representing the PlotReference to look for
#' @return character vector of data types matching the provided plot reference
#' @export
setGeneric("findDataTypesFromPlotRef", 
  function(variables, plotRef) standardGeneric("findDataTypesFromPlotRef"),
  signature = "variables"
)

#' @export
setMethod("findDataTypesFromPlotRef", signature("VariableMetadataList"), function(variables, plotRef) {
  if (is.null(variables)) return(NULL)

  dataTypes <- purrr::map(variables, function(x) { if(x@plotReference@value == plotRef) { return(x@dataType@value) } })

  return(veupathUtils::toStringOrNull(dataTypes))
})

#' EDA Variable Data Shapes matching a PlotReference
#' 
#' This function provides EDA-compliant data shapes provided
#' an EDA-compliant VariableMetadataList object.
#' @param variables a VariableMetadataList of variables to search
#' @param plotRef a string representing the PlotReference to look for
#' @return character vector of data shapes matching the provided plot reference
#' @export
setGeneric("findDataShapesFromPlotRef", 
  function(variables, plotRef) standardGeneric("findDataShapesFromPlotRef"),
  signature = "variables"
)

#' @export
setMethod("findDataShapesFromPlotRef", signature("VariableMetadataList"), function(variables, plotRef) {
  if (is.null(variables)) return(NULL)

  dataTypes <- purrr::map(variables, function(x) { if(x@plotReference@value == plotRef) { return(x@dataShape@value) } })

  return(veupathUtils::toStringOrNull(dataTypes))
})

#' EDA Variable Column Name of a VariableSpec
#'
#' This function provides an EDA-compliant column name for a 
#' particular VariableSpec.
#' @param varSpec a VariableSpec to find the column name for
#' @return a character vector of length one
#' @export
setGeneric("getColName", 
  function(varSpec) standardGeneric("getColName"),
  signature = "varSpec"
)

#' @export
setMethod("getColName", signature("VariableSpec"), function(varSpec) {
  return(veupathUtils::toStringOrNull(paste0(varSpec@entityId, ".", varSpec@variableId)))
})

#' Find EDA column names
#' 
#' @param variables a VariableMetadataList
#' @param predicateFunction a function to identify which VariableMetadata objects to return column names for
#' @return character vector of column names for VariableMetadata objects matching the predicate
#' @importFrom purrr map
#' @export
setGeneric("findColNamesByPredicate", 
  function(variables, predicateFunction) standardGeneric("findColNamesByPredicate"),
  signature = "variables"
)

#' @export
setMethod("findColNamesByPredicate", signature("VariableMetadataList"), function(variables, predicateFunction) {
  if(is.null(variables)) return(NULL)

  # For each variable in the variable list, return the column name if the predicate is true for that variable
  colNames <- purrr::map(variables, function(x) {if (identical(predicateFunction(x), TRUE)) {return(veupathUtils::getColName(x@variableSpec))}})
  colNames <- unlist(colNames)

  return (colNames)
})