check_bin_range <- function(object) {
    errors <- character()
    start <- object@binStart
    end <- object@binEnd
    label <- object@binLabel
    value <- object@value
    
    if (sum(is.na(c(start, end))) == 1 || sum(c(!length(start), !length(end))) == 1) {
      msg <- "Must provide either both binStart and binEnd or neither."
      errors <- c(errors, msg) 
    }

    if (any(c(length(start), length(end), length(label), length(value)) > 1)) {
      msg <- "More than one value provided for one of binStart, binEnd, binLabel or value."
      errors <- c(errors, msg)
    } else {
      if (!any(is.na(c(start, end))) && any(is.na(suppressWarnings(as.numeric(c(start, end)))))) {
        msg <- "Must provide binStart and binEnds which are coercible to numeric."
        errors <- c(errors, msg)
      } else {
        if (!any(is.na(c(start, end))) && as.numeric(start) > as.numeric(end)) {
          msg <- "Provided binStart is after BinEnd."
          errors <- c(errors, msg)
        }
      }

      if (!length(label) || is.na(label)) {
        msg <- "Must provide a binLabel."
        errors <- c(errors, msg)
      }
    }

    if (class(start) != class(end)) {
      msg <- "Provided binStart and binEnd must be of the same base type (numeric, Date, Posix)."
      errors <- c(errors, msg) 
    }   

    return(if (length(errors) == 0) TRUE else errors)
}

#' Bin
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
#' @name Bin-class
#' @rdname Bin-class
#' @export 
Bin <- setClass("Bin", representation(
    binStart = 'ANY',
    binEnd = 'ANY',
    binLabel = 'character',
    value = 'numeric'
), prototype = prototype(
    binStart = NA_real_,
    binEnd = NA_real_,
    value = NA_real_
), validity = check_bin_range)

check_bin_range_list <- function(object) {
  errors <- character()
  binStarts <- as.numeric(unlist(lapply(as.list(object), FUN = function(bin) {bin@binStart})))
  binEnds <- as.numeric(unlist(lapply(as.list(object), FUN = function(bin) {bin@binEnd})))
  binValues <- as.numeric(unlist(lapply(as.list(object), FUN = function(bin) {bin@value})))
  
  anyBinStart <- any(!is.na(binStarts))
  anyBinEnd <- any(!is.na(binEnds))
  allBinStart <- all(!is.na(binStarts))
  allBinEnd <- all(!is.na(binEnds))

  # if any have bin start, end then all have both
  if ((anyBinStart || anyBinEnd) && !(allBinStart && allBinEnd)) {
    msg <- "Some provided bins include start or ends but not others."
    errors <- c(errors, msg)
  } else if (anyBinStart) {
    # no dups
    if (anyDuplicated(binStarts)) {
      msg <- "Duplicate binStart values found."
      errors <- c(errors, msg)
    }

    if (anyDuplicated(binEnds)) {
      msg <- "Duplicate binEnd values found."
      errors <- c(errors, msg)
    }

    # start always before end
    if (any(binStarts > binEnds)) {
      msg <- "At least one binStart is after its associated binEnd."
      errors <- c(errors, msg)
    }
  
    # no overlapping ranges
    sortOrder <- order(binStarts)
    binStarts <- binStarts[sortOrder]
    binEnds <- binEnds[sortOrder]
    binValues <- binValues[sortOrder]
    if (any(binStarts[2:length(binStarts)] < binEnds[1:(length(binEnds)-1)], na.rm=TRUE)) {
      msg <- "Some provided bins overlap."
      errors <- c(errors, msg) 
    }
  }

  # if any have a value then all do
  anyValues <- any(!is.na(binValues))
  allValues <- all(!is.na(binValues))
  if (anyValues && !allValues) {
    msg <- "Some provided bins include values but not others."
    errors <- c(errors, msg)
  }

  return(if (length(errors) == 0) TRUE else errors)
}

#' @export
BinList <- setClass("BinList",
  contains = "SimpleList",
  prototype = prototype(elementType = "Bin"),
  validity = check_bin_range_list
)