

#' Make Aggregation Formulas
#' 
#' This function is intended to help create formulas for input to
#' base R's `aggregate` function. 
#' @param LHS vector of variable/ column names to become the LHS
#' @param RHS vector of variable/ column names to become the RHS
#' @return character string which can be used as input to `aggregate` with `as.formula`
#' @export
getAggStr <- function(LHS = NULL, RHS = NULL) {
  LHS <- toStringOrPoint(paste(c(LHS), collapse= " + "))
  RHS <- toStringOrNull(paste(c(RHS), collapse=" + "))
  aggStr <- paste(c(LHS, RHS), collapse=" ~ ")

  return(aggStr)
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

#' Set Row Order
#' 
#' setroworder reorders the rows of a data.table to the new order provided.
#' @param a data.table
#' @param neworder Character vector of the new column name ordering. 
#' @export
setroworder <- function(x, neworder) {
    .Call(data.table:::Creorder, x, as.integer(neworder), PACKAGE = "data.table")
    invisible(x)
}