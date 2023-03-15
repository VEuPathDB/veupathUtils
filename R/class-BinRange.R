check_bin_range <- function(object) {
    errors <- character()
    start <- object@binStart
    end <- object@binEnd
    label <- object@binLabel
    
    if (class(start) != class(end)) {
       msg <- "Provided binStart and binEnd must be of the same base type (numeric, Date, Posix)."
       errors <- c(errors, msg) 
    }

    if (!inherits(start, c('numeric', 'Date', 'Posix'))) {
       msg <- "Provided binStart and binEnd must be numeric, Date, or Posix."
       errors <- c(errors, msg) 
    }

    if (!length(label) || is.na(label)) {
      msg <- "Must provide a binLabel."
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

#' Bin Range
#' 
#' A class to define the start, end and label for a bin.
#' Optionally, may include a value for that bin.
#' If no start and end are provided, then assume the 'bin'
#' represents a category.
#' 
#' @slot binStart A number, Date or Posix value
#' @slot binEnd A number, Date or Posix value
#' @slot binLabel A string
#' @slot value
#' 
#' @name BinRange-class
#' @rdname BinRange-class
#' @export 
BinRange <- setClass("BinRange", representation(
    binStart = 'ANY',
    binEnd = 'ANY',
    binLabel = 'character',
    value = 'numeric'
), prototype = prototype(
    binStart = NA_real_,
    binEnd = NA_real_,
    value = NA_real_
), validity = check_bin_range)

#' @export
BinRangeList <- setClass("BinRangeList",
  contains = "SimpleList",
  prototype = prototype(elementType = "BinRange")
)