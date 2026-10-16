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
#' This function efficiently splits each string in a vector at its first match of
#' `pattern`, returning either the part before it (`index = 1`) or everything after
#' it (`index = 2`). Any further occurrences of `pattern` are left in the second part,
#' so `strSplit("entity.gene.1", ".", index = 2)` is `"gene.1"`.
#' @param str Character vector of strings to split
#' @param pattern Character string or regular expression to split str on
#' @param ncol Deprecated and ignored. Retained so existing calls keep working.
#' @param index Number indicating which part is wanted: 1 (before the first match)
#' or 2 (after the first match)
#' @param fixed Boolean indicating if `pattern` is a literal string (TRUE)
#' or regular expression (FALSE)
#' @return Character vector of substrings, one per element of `str`. If a string
#' contains no match, `index = 1` returns the whole string and `index = 2` returns `NA`.
#' @export
strSplit <- function(str, pattern, ncol = 2, index = 1, fixed = TRUE) {
  if (!length(index) == 1L || !index %in% c(1, 2)) {
    stop("strSplit only splits at the first match, so index must be 1 or 2.")
  }

  str <- as.character(str)
  firstMatch <- regexpr(pattern, str, fixed = fixed)
  matchStart <- as.integer(firstMatch)
  matchLength <- attr(firstMatch, "match.length")
  hasMatch <- !is.na(matchStart) & matchStart > 0L

  if (index == 1) {
    return(ifelse(hasMatch, substr(str, 1L, matchStart - 1L), str))
  }

  ifelse(hasMatch, substring(str, matchStart + matchLength), NA_character_)
}