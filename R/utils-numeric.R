## TODO think about the relationship between this and the plot.data bin fxns
#' Find Bin Ranges
#' 
#' This function will find bin start, end and labels for a 
#' continuous variable. Optionally, it can return a value/ count
#' per bin. By default returns 10 bins for equalRanges and quantile
#' methods and 6 for sd (standard deviations).
#' @param x Numeric (or Date) vector to find bins for
#' @param method A string indicating which method to use to find bins ('equalRanges', 'quantile', 'sd')
#' @param numBins A number indicating how many bins are desired
#' @param getValue A boolean indicating whether to return the counts per bin
#' @export 
getBinRanges <- function(x, method = c('equalInterval', 'quantile', 'sd'), numBins = NULL, getValue = c(TRUE, FALSE)) {
  #validate inputs, if numBins is NULL set it
  #if method is sd, ignore numBins?

  binEdges <- unname(breaks(x, method, numBins))
  if (method == 'quantile' && anyDuplicated(binEdges)) {
    ## TODO decide if we prefer this, or an error, for UX
    warning("There is insufficient data to produce the requested number of bins. Returning as many bins as possible.")
    binEdges <- unique(binEdges)
  }

  binStart <- formatC(binEdges[1:(length(binEdges)-1)])
  binEnd <- formatC(binEdges[2:length(binEdges)])
  binLabel <- paste0("(",binStart,", ", binEnd, "]")
  binLabel[[1]] <- gsub("(","[",binLabel[[1]], fixed=T)

  if (getValue) {
    value <- c(table(cut(x, binEdges, include.lowest=TRUE)))
  } else {
    value <- rep(NA_real_, length(binStart))
  }
  
  binRanges <- lapply(1:length(binStart), FUN = function(x) { BinRange(binStart = binStart[[x]],
                                                                       binEnd = binEnd[[x]],
                                                                       binLabel = binLabel[[x]],
                                                                       value = value[[x]]) })

  return(BinRangesList(S4Vectors::SimpleList(binRanges)))
}

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

#' Replace numeric NAs with 0 - update by reference
#'
#' This function replaces NAs in numeric columns with 0.
#' @param x data.table, data.frame, or list
#' @param cols vector of column names for which the NA replacement should occur.
#' Default is all numeric columns.
#' @return x with desired NAs replaced with 0.
#' @export
setNaToZero <- function(df, cols = NULL) {

  # if cols not set, use all numeric cols
  if (is.null(cols)) cols <- findNumericCols(df)
  cols <- validateNumericCols(df, cols=cols)

  data.table::setnafill(df, fill = 0, cols = cols)
}


#' Replace numeric NAs with 0
#'
#' This function replaces numeric NAs with 0.
#' @param x list, data.frame, array, or vector
#' @param cols Optional. When appropriate, vector of column names 
#' for which the NA replacement should occur.
#' Default is all numeric columns.
#' @return object the same type as x, where desired NAs are replaced with 0.
#' @export
naToZero <- function(x, ...) {
  UseMethod("naToZero")
}

#' @export
naToZero.data.table <- function(x, cols = NULL) {

  # if cols not set, use all numeric cols
  if (is.null(cols)) cols <- findNumericCols(x)
  cols <- validateNumericCols(x, cols)

  x[, cols] <- x[, ..cols][, lapply(.SD, function(y){y[is.na(y)] <- 0; y})]
  return(x)
}

#' @export
naToZero.data.frame <- function(x, cols = NULL) {

  # if cols not set, use all numeric cols
  if (is.null(cols)) cols <- findNumericCols(x)
  cols <- validateNumericCols(x, cols)

  x[cols][is.na(x[cols])] <- 0
  return(x)
}

#' @export
naToZero.list <- function(x, cols = NULL) {

  # if cols not set, use all numeric cols
  if (is.null(cols)) cols <- findNumericCols(x)
  cols <- validateNumericCols(x, cols)

  x[cols] <- lapply(x[cols], function(y) {y[is.na(y)] <- 0; return(y)})
  return(x)
}

#' @export
naToZero.default <- function(x) {
  numericEntries <- purrr::map_lgl(x, is.numeric)
  x[numericEntries][is.na(x[numericEntries])] <- 0
  return(x)
}


#' Find numeric columns
#'
#' This function finds all numeric columns in a list
#' @param x list, data.frame, array, or vector
#' @return vector of numeric column names in x. If no numeric columns found, returns NULL
#' @importFrom purrr map_lgl
findNumericCols <- function(x) {
  numericCols <- names(x)[purrr::map_lgl(x, is.numeric)]
  
  # If no numeric cols, return NULL
  if (!length(numericCols)) return(NULL)

  return(numericCols)
}


#' Validate numeric columns
#' 
#' Given a vector of column names or indices, this function ensures all
#' referenced columns are numeric
#' @param x list, data.frame, array, data.table
#' @param cols vector of column names or column indices. NAs will be removed.
#' @return given column names
#' @importFrom purrr map_lgl
#' @import data.table
#' @export
validateNumericCols <- function(x, cols, ...) {
  UseMethod("validateNumericCols")
}

#' @importFrom purrr map_lgl
#' @export
validateNumericCols.data.table <- function(x, cols) {
  if (any(is.na(cols))) {cols <- cols[!is.na(cols)]; warning("validateNumericCols warning: NAs in cols removed")}
  if (!length(cols)) {warning("validateNumericCols warning: no numeric columns given"); return(cols)}
  if (is.character(cols)) {
    if (!all(cols %in% names(x))) stop('validateNumericCols failed: Column name not found in the input')
  } else {
    if (max(cols) > ncol(x) | min(cols) < 1) stop('validateNumericCols failed: column index does not represent a valid column')
  }
  if (!all(purrr::map_lgl(x[, ..cols], is.numeric))) stop('validateNumericCols failed: All columns must be numeric')
  return(cols)
}

#' @importFrom purrr map_lgl
#' @export
validateNumericCols.list <- function(x, cols) {
  if (any(is.na(cols))) {cols <- cols[!is.na(cols)]; warning("validateNumericCols warning: NAs in cols removed")}
  if (!length(cols)) {warning("validateNumericCols warning: no numeric columns given"); return(cols)}
  if (is.character(cols)) {
    if (!all(cols %in% names(x))) stop('validateNumericCols failed: Column name not found in the input')
  } else {
    if (max(cols) > length(names(x)) | min(cols) < 1) stop('validateNumericCols failed: column index does not represent a valid column')
  }
  if (!all(purrr::map_lgl(x[cols], is.numeric))) stop('validateNumericCols failed: All columns must be numeric')
  return(cols)
}


#' @importFrom purrr map_lgl
#' @export
validateNumericCols.default <- function(x, cols) {
  if (any(is.na(cols))) {cols <- cols[!is.na(cols)]; warning("validateNumericCols warning: NAs in cols removed")}
  if (!length(cols)) {warning("validateNumericCols warning: no numeric columns given"); return(cols)}
  if (is.character(cols)) {
    if (!all(cols %in% names(x))) stop('validateNumericCols failed: Column name not found in the input')
  } else {
    if (max(cols) > ncol(x) | min(cols) < 1) stop('validateNumericCols failed: column index does not represent a valid column')
  }
  if (!all(purrr::map_lgl(x[cols], is.numeric))) stop('validateNumericCols failed: All columns must be numeric')
  return(cols)
}
