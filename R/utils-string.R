#' Replace Empty String with NULL
#'
#' This function replaces the empty string "" with NULL 
#' @param x character vector
#' @return non-empty character vector or NULL
#' @export
toStringOrNull <- function(x = NULL, na.rm = c(TRUE, FALSE)) {
  x <- unlist(x)
  if (is.null(x)) { return(NULL) }
  if (!length(x)) { return(NULL) }
  
  na.rm <- matchArg(na.rm)
  if (!na.rm) { x[is.na(x)] <- "NA" }

  if (all(is.na(x))) { 
    return(NULL) 
  } else if (any(is.na(x))) {
    x <- x[complete.cases(x)]
  }

  x <- as.character(x)
  if (all(x == "")) { 
    return(NULL) 
  } else if (any(x == "")) {
    x <- x[x != ""]
  }

  return(x)
}

# does this one need a collapse arg? maybe you want one point for each empty in a vector?
#' @export
toStringOrPoint <- function(x = ".", na.rm = c(TRUE, FALSE)) {
  x <- unlist(x)
  if (is.null(x)) { return(".") }
  if (!length(x)) { return(".") }
  
  na.rm <- matchArg(na.rm)
  if (!na.rm) { x[is.na(x)] <- "NA" }

  if (all(is.na(x))) { 
    return(NULL) 
  } else if (any(is.na(x))) {
    x <- x[complete.cases(x)]
  }

  x <- as.character(x)
  if (all(x == "")) { return(".") }

  return(x)
}

#' Trim White Space
#' 
#' Function to trim white space from a string.
#' @param x Character vector to trim white space from.
#' @return character vector of trimmed values
#' @export
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' Efficient Vectorized String Split
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
  # do we want an arg to not apply the index and return multiple cols? in a data.frame?
  # should we return a list if we received one?
  matrix(unlist(strsplit(str, pattern, fixed = fixed)), ncol = ncol, byrow = TRUE)[,index]
}