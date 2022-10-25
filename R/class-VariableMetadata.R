variable_classes <- c('native', 'derived', 'computed')
plot_references <- c('xAxis', 'yAxis', 'zAxis', 'overlay', 'facet')
data_types <- c('NUMBER', 'STRING', 'INTEGER', 'DATE', 'LONGITUDE')
data_shapes <- c('CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY')

# these are annoying bc im essentially doing this to enforce an enum type in R
# maybe see if theres a better way
check_variable_class <- function(object) {
    errors <- character()
    variable_class <- object@value
    
    if (length(variable_class) != 1) {
      msg <- "Variable class must have a single value."
      errors <- c(errors, msg) 
    }

    if (suppressWarnings(!variable_class %in% variable_classes)) {
      msg <- paste("Variable class must be one of", paste(variable_classes, collapse = ", "))
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

check_plot_reference <- function(object) {
    errors <- character()
    plot_reference <- object@value
    
    if (length(plot_reference) != 1) {
      msg <- "Plot reference must have a single value."
      errors <- c(errors, msg) 
    }

    if (suppressWarnings(!plot_reference %in% plot_references)) {
      msg <- paste("Plot reference must be one of", paste(plot_references, collapse = ", "))
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

check_data_type <- function(object) {
    errors <- character()
    data_type <- object@value
    
    if (length(data_type) != 1) {
      msg <- "Data type must have a single value."
      errors <- c(errors, msg) 
    }

    if (suppressWarnings(!data_type %in% data_types)) {
      msg <- paste("Data type must be one of", paste(data_types, collapse = ", "))
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

check_data_shape <- function(object) {
    errors <- character()
    data_shape <- object@value

    if (length(data_shape) != 1) {
      msg <- "Data shape must have a single value."
      errors <- c(errors, msg) 
    }

    if (suppressWarnings(!data_shape %in% data_shapes)) {
      msg <- paste("Data shape must be one of", paste(data_shapes, collapse = ", "))
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

setClass("VariableClass", representation(
    value = 'character'
), prototype = prototype(
    value = NA_character_
), validity = check_variable_class)

# should probably go through slots, make a list of contents
# call toJSON on each item of the list somehow? or parse the contents to a list same as the parent? (relevant for S4 children)
# for now just never use the generic, its a placeholder
setGeneric("toJSON", function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    return(jsonlite::toJSON(object))
})

setMethod("toJSON", signature("VariableClass"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- jsonlite::toJSON(object@value)

    if (named) tmp <- paste0('{"variableClass":', tmp, '}')
    
    return(tmp)
})

setClass("VariableSpec", representation(
    variableId = "character",
    entityId = "character"
), prototype = prototype(
    variableId = NA_character_,
    entityId = NA_character_
))

setMethod("toJSON", signature("VariableSpec"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- list("variableId" = jsonlite::unbox(object@variableId),
                "entityId" = jsonlite::unbox(object@entityId))

    if (named) tmp <- list("variableSpec" = tmp)

    return(jsonlite::toJSON(tmp))
})

setClass("PlotReference", representation(
  value = 'character'
), prototype = prototype(
  value = NA_character_
), validity = check_plot_reference)

setMethod("toJSON", signature("PlotReference"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- jsonlite::toJSON(object@value)

    if (named) tmp <- paste0('{"plotReference":', tmp, '}')
    
    return(tmp)
})

#' @importFrom S4Vectors SimpleList
setClass("VariableSpecList", 
  contains = "SimpleList", 
  prototype = prototype(elementType = "VariableSpec")
)

# helper S4Vectors SimpleList toJSON
# no easy option to be named here really i guess
S4SimpleListToJSON <- function(S4SimpleList) {
    if (!inherits(S4SimpleList, 'SimpleList')) stop("S4SimpleListToJSON only accepts an S4Vectors::SimpleList as input.", class(S4SimpleList), "was provided.")

    tmp <- as.list(S4SimpleList)
    tmp <- lapply(tmp, veupathUtils::toJSON, FALSE)
    tmp <- paste(tmp, collapse = ",")
    tmp <- paste0('[', tmp, ']')

    return(tmp)
}


setMethod("toJSON", signature("VariableSpecList"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- S4SimpleListToJSON(object)

    if (named) tmp <- paste0('{"variableSpecs":', tmp, "}")

    return(tmp)
})

setClass("DataType", representation(
    value = 'character'
), prototype = prototype(
    value = NA_character_
), validity = check_data_type)

setMethod("toJSON", signature("DataType"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- jsonlite::unbox(object@value)

    if (named) tmp <- list("dataType" = tmp)
    
    return(jsonlite::toJSON(tmp))
})

setClass("DataShape", representation(
    value = 'character'
), prototype = prototype(
    value = NA_character_
), validity = check_data_shape)

setMethod("toJSON", signature("DataShape"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- jsonlite::unbox(object@value)

    if (named) tmp <- list("dataShape" = tmp)
    
    return(jsonlite::toJSON(tmp))
})

check_variable_metadata <- function(object) {
    data_type <- object@dataType@value
    min <- object@displayRangeMin
    max <- object@displayRangeMax
    variable_spec <- object@variableSpec
    variable_class <- object@variableClass@value
    plot_reference <- object@plotReference@value

    errors <- character()
    # class, varId and entityId must be non-empty
    if (!length(variable_class)) errors <- c(errors, "Variable class must be non-empty.")
    
    if (!length(variable_spec)) {
      errors <- c(errors, "VariableSpec must be non-empty.")
    } else {
        if (!length(variable_spec@variableId)) errors <- c(errors, "Variable Id must be non-empty.")
        if (!length(variable_spec@entityId)) errors <- c(errors, "Entity Id must be non-empty.")
    }

    if (!length(plot_reference)) errors <- c(errors, "Plot reference must be non-empty.")

    # need display ranges, vocab etc for derived and computed vars
    if (any(c('derived', 'computed') %in% variable_class)) {
      if (is.na(object@displayName)) errors <- c(errors, "Display name must be non-empty for derived or computed variables.")

      # display range min/max must be numeric for numeric types, string for dates, NULL else
      if (is.na(min) && data_type != "STRING") {
        errors <- c(errors, "Display range min must be non-empty for derived or computed variables.")
      } else {
        if (data_type == "NUMBER" && !is.numeric(min)) errors <- c(errors, "Display range min must be numeric for data type 'NUMBER'.")
        if (data_type == "DATE" && !is.character(min)) errors <- c(errors, "Display range min must be of type character for data type 'DATE'.")
        if (data_type == "STRING" && !is.na(min)) errors <- c(errors, "Display range min must be NA for data type 'STRING'")
      }

      if (is.na(max) && data_type != "STRING") {
        errors <- c(errors, "Display range max must be non-empty for derived or computed variables.")
      } else {
        if (data_type == "NUMBER" && !is.numeric(max)) errors <- c(errors, "Display range max must be numeric for data type 'NUMBER'.")
        if (data_type == "DATE" && !is.character(max)) errors <- c(errors, "Display range max must be of type character for data type 'DATE'.")
        if (data_type == "STRING" && !is.na(max)) errors <- c(errors, "Display range max must be NA for data type 'STRING'")
      }

    }

    # need members only for collections
    if ('collection' %in% variable_class && !length(object@members)) errors <- c(errors, "Members must be non-empty for collection variables.")

    return(if (length(errors) == 0) TRUE else errors)
}

setClass("VariableMetadata", representation(
    variableClass = 'VariableClass',
    variableSpec = 'VariableSpec',
    plotReference = 'PlotReference',
    displayName = 'character',
    displayRangeMin = 'ANY',
    displayRangeMax = 'ANY',
    dataType = 'DataType',
    dataShape = 'DataShape',
    vocabulary = 'character',
    isCollection = 'logical',
    imputeZero = 'logical',
    members = 'VariableSpecList'
), prototype = prototype(
    displayName = NA_character_,
    displayRangeMin = NA_real_,
    displayRangeMax = NA_real_,
    vocabulary = NA_character_,
    isCollection = FALSE,
    imputeZero = FALSE
), validity = check_variable_metadata)

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

# mostly for convenience writing json
setClass("VariableMetadataList",
  contains = "SimpleList",
  prototype = prototype(elementType = "VariableMetadata")
)

setMethod("toJSON", signature("VariableMetadataList"), function(object, named = c(TRUE, FALSE)) {
    named <- veupathUtils::matchArg(named) 
    tmp <- S4SimpleListToJSON(object)

    if (named) tmp <- paste0('{"variables":', tmp, "}")

    return(tmp)
})