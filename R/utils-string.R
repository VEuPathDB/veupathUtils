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

#' Trim White Space
#' 
#' Function to trim white space from a string.
#' @param x Character vector to trim white space from.
#' @return character vector of trimmed values
#' @export
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

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