# Utils dump

#' Replace Empty String with NULL
#'
#' This function replaces the empty string "" with NULL 
#' @param x character vector
#' @return non-empty character vector or NULL
#' @export
emptyStringToNull <- function(x) {
  x <- unlist(x)
  if (is.null(x)) { return(NULL) }
  if (length(x) == 0) { return(NULL) }
  if (all(x == "")) { return(NULL) }

  return(as.character(x))
}

#' POSIXct Test
#'
#' This function returns a logical value indicating if x is
#' a POSIXct object.
#' @param x an R object
#' @return logical TRUE if x is a POSIXct object, FALSE otherwise
#' @export
is.POSIXct <- function(x) inherits(x, "POSIXct")

#' Trim White Space
#' 
#' Function to trim white space from a string.
#' @param x Character vector to trim white space from.
#' @return character vector of trimmed values
#' @export
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' Non-Zero Rounding
#' 
#' This function will recursively attempt to round a value to
#' greater and greater precision until it results in a non-zero
#' value. One consequence of this is that the precision of the
#' output value may not be exactly what was requested.
#' @param x Numeric value to round
#' @param digits Number indicating the desired precision
#' @return number rounded as nearly to the requested precision as 
#' possible without returning zero.
#' @export
nonZeroRound <- function(x, digits) {
  if (x == 0) {
    warning("Input is already zero and cannot be rounded to a non-zero number.")
    return(x)
  }
  if (round(x,digits) == 0) { 
    Recall(x,digits+1) 
  } else { 
    round(x,digits) 
  } 
}

#' Make Aggregation Formulas
#' 
#' This function is intended to help create formulas for input to
#' base R's `aggregate` function. 
#' @param valueVars vector of variable/ column names to become the LHS
#' @param groupingVars vector of variable/ column names to become the RHS
#' @return character string which can be used as input to `aggregate` with `as.formula`
#' @export
getAggStr <- function(valueVars, groupingVars) {
  valueString <- emptyStringToPoint(paste(valueVars, collapse= " + "))
  groupingString <- emptyStringToNull(paste(groupingVars, collapse=" + "))
  aggStr <- paste(c(valueString, groupingString), collapse=" ~ ")

  return(aggStr)
}

#' @export
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    stop("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      stop("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

#' Fast data.frame
#' 
#' This function should be used with extreme caution. If data.frame is not 
#' performant enough, you should consider data.table instead. This is 
#' intended to meet a very specific use case where a solution is needed which
#' is both efficient and tidy (meaning tidyverse compatible). It comes at the
#' cost of cutting some corners. There is no checking, recycling etc. unless asked for.
#' @param ... usual arguments to data.frame
#' @export
data_frame <- function(...) {
  new_data_frame(list(...))
}

#' Efficient String Split
#'
#' This function efficiently splits strings by a vectorized approach.
#' @param str Character vector or list of strings to split
#' @param pattern Character string or regular expression to split str on
#' @param ncol Number of anticipated output values/ columns after splitting
#' @param index Number indicating index of value/ column of interest
#' @param fixed Boolean indicating if `pattern` is a literal string (TRUE) 
#' or regular expression (FALSE)
#' @return Character vector of substrings
#' @export
strSplit <- function(str, pattern, ncol = 2, index = 1, fixed = TRUE) {
  matrix(unlist(strsplit(str, pattern, fixed = fixed)), ncol = ncol, byrow = TRUE)[,index]
}

#' Try-error Test
#'
#' This function returns a logical value indicating if x is
#' a try-error object.
#' @param x an R object
#' @return logical TRUE if x is a try-error object, FALSE otherwise
#' @export
is.error <- function(x) inherits(x, "try-error")


#' Set object attributes from a list
#'
#' This function sets the attributes of an object based on a given list
#' @param .dt data.table
#' @param attr named list of desired attributes to add to .dt
#' @param removeExtraAttrs boolean indicating if attributes of .dt not included in attr should be removed.
#' @import data.table
#' @export
setAttrFromList <- function(.dt, attr, removeExtraAttrs=T) {
  
  
  if (!is.data.table(.dt)) {
    stop(".dt must be of class data.table")
  }
  
  # If removeExtraAttrs=T, remove any .dt attribute not in attr
  if (removeExtraAttrs) {
    attrNames <- names(attributes(.dt))
    attrToRemove <- attrNames[!(attrNames %in% names(attr))]
    
    if (length(attrToRemove) > 0) {
      invisible(lapply(attrToRemove, removeAttr, .dt))
    }
  }
  
  # For each item in the attr list, add to .dt attributes or update existing
  invisible(lapply(seq_along(attr), updateAttrById, attr, .dt))
  
  return(.dt)
}

#' @export
removeAttr <- function(attrToRemove, .dt) {
  data.table::setattr(.dt, attrToRemove, NULL)
  return(NULL)
}

#' @export
updateAttrById <- function(attrInd, attr, .dt) {
  data.table::setattr(.dt, names(attr)[attrInd], attr[[attrInd]])
  return(NULL)
}

#' Diagnositc Messages with Time of Occurance
#'
#' This function generates a diagnositc message which
#' includes the time of occurance.
#' @param message character to pass to `message`
#' @param verbose boolean indicating if timed logging is desired
#' @export
logWithTime <- function(message, verbose) {
  if (verbose) {
    message('\n', Sys.time(), ' ', message)
  }
}


#' Character and Logical Argument Verification
#'
#' `matchArg` matches `arg` against a table of candidates values as
#' specified by `choices`, where `NULL` means to take the first one.
#'
#' In the one-argument form `matchArg(arg)`, the choices are
#' obtained from a default setting for the formal argument `arg` of
#' the function from which `matchArg` was called.  (Since default
#' argument matching will set `arg` to `choices`, this is allowed as
#' an exception to the "length one unless `several.ok` is `TRUE`"
#' rule, and returns the first element.)
#' @param arg a character vector of length one
#' @param choices a character vector of candidate values
#' @return The unabbreviated version of the exact match
#' @importFrom  stringi stri_detect_regex
#' @export
matchArg <- function(arg, choices) {
  
  # If choices is not supplied, extract from function definition
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]])
  }

  # Return first value as default
  if (is.null(arg)) return(choices[1L])
  if (identical(arg, choices)) return(arg[1L])

  # Validate inputs
  if (!identical(typeof(arg), typeof(choices))) {
    stop("'arg' must be of the same type as 'choices'.")
  }
  if (length(arg) != 1L) stop("'arg' must be of length 1")
  if (!is.character(arg) && !is.logical(arg)) {
     stop("'arg' must be NULL, a character vector, or a logical vector.")
  }
  
  # Perform argument matching based on type
  if (is.character(arg)) {

    # If arg does not match any values in choices, err. Otherwise, arg must have matched.
    if (!any(stringi::stri_detect_regex(choices, paste0('^', arg, '$')))) {
      stop(gettextf("'arg' should be one of %s", paste(dQuote(choices), collapse = ", ")), domain = NA)
    }
    
  } else if (is.logical(arg)) {

    # If arg does not match any values in choices, err. Otherwise, arg must have matched.
    if (!(arg %in% choices)) {
      stop("'arg' does not match any value in 'choices'")
    }
  }

  return (arg)
}