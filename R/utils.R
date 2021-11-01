# Utils dump

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