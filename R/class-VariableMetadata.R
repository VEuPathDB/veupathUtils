# need to decide if you can have multiple values, or a hierarchy?
# ex: a computed collection 
# or maybe collection isnt a class but something else?
variable_classes <- c('native', 'derived', 'computed', 'collection')
# TODO check if any are missing here
data_types <- c('NUMBER', 'STRING', 'INTEGER', 'DATE')
data_shapes <- c('CONTINUOUS', 'CATEGORICAL', 'ORDINAL')

# these are annoying bc im essentially doing this to enforce an enum type in R
# maybe see if theres a better way
check_variable_class <- function(object) {
    errors <- character()
    variable_class <- object@value
    if (!variable_class %in% variable_classes) {
      msg <- paste("Variable class must be one of", paste(variable_classes, collapse = ", "))
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


setClass("VariableSpec", representation(
    variableId = "character",
    entityId = "character"
), prototype = prototype(
    variableId = NA_character_,
    entityId = NA_character_
))

#' @importFrom S4Vectors SimpleList
setClass("VariableSpecList", 
  contains = "SimpleList", 
  prototype = prototype(elementType = "VariableSpec")
)

setClass("DataType", representation(
    value = 'character'
), prototype = prototype(
    value = NA_character_
), validity = check_data_type)

setClass("DataShape", representation(
    value = 'character'
), prototype = prototype(
    value = NA_character_
), validity = check_data_shape)

check_variable_metadata <- function(object) {
    data_type <- object@dataType@value
    min <- object@displayRangeMin
    max <- object@displayRangeMax
    variable_spec <- object@variableSpec
    variable_class <- object@variableClass@value

    errors <- character()
    # class, varId and entityId must be non-empty
    if (!length(variable_class)) errors <- c(errors, "Variable class must be non-empty.")
    
    if (!length(variable_spec)) {
      errors <- c(errors, "VariableSpec must be non-empty.")
    } else {
        if (!length(variable_spec@variableId)) errors <- c(errors, "Variable Id must be non-empty.")
        if (!length(variable_spec@entityId)) errors <- c(errors, "Entity Id must be non-empty.")
    }

    # need display ranges, vocab etc for derived and computed vars
    if (any(c('derived', 'computed') %in% variable_class)) {
      if (!length(object@displayName)) errors <- c(errors, "Display name must be non-empty for derived or computed variables.")

      # display range min/max must be numeric for numeric types, string for dates, NULL else
      if (!length(min) && data_type != "STRING") {
        errors <- c(errors, "Display range min must be non-empty for derived or computed variables.")
      } else {
        if (data_type == "NUMBER" && !is.numeric(min)) errors <- c(errors, "Display range min must be numeric for data type 'NUMBER'.")
        if (data_type == "DATE" && !is.character(min)) errors <- c(errors, "Display range min must be of type character for data type 'DATE'.")
      }

      if (!length(max) && data_type != "STRING") {
        errors <- c(errors, "Display range max must be non-empty for derived or computed variables.")
      } else {
        if (data_type == "NUMBER" && !is.numeric(max)) errors <- c(errors, "Display range max must be numeric for data type 'NUMBER'.")
        if (data_type == "DATE" && !is.character(max)) errors <- c(errors, "Display range max must be of type character for data type 'DATE'.")
      }

    }

    # need members only for collections
    if ('collection' %in% variable_class && !length(object@members)) errors <- c(errors, "Members must be non-empty for collection variables.")

    return(if (length(errors) == 0) TRUE else errors)
}

setClass("VariableMetadata", representation(
    variableClass = 'VariableClass',
    variableSpec = 'VariableSpec',
    displayName = 'character',
    displayRangeMin = 'ANY',
    displayRangeMax = 'ANY',
    dataType = 'DataType',
    dataShape = 'DataShape',
    vocabulary = 'character',
    members = 'VariableSpecList'
), prototype = prototype(
    displayName = NA_character_,
    displayRangeMin = NA_real_,
    displayRangeMax = NA_real_,
    vocabulary = NA_character_
), validity = check_variable_metadata)

# do we need this, or just make a list of the other obj
# might depend how easy it is to write these w jsonlite how we like
setClass("VariableMetadataList",
  contains = "SimpleList",
  prototype = prototype(elementType = "VariableMetadata")
)
