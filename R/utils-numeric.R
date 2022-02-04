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
#' @importFrom purrr map_lgl
#' @export
setNaToZero <- function(df, cols = NULL) {


  if (is.null(cols)) {
    # if cols not set, use all numeric cols
    cols <- names(df)[purrr::map_lgl(df, is.numeric)]
  } else {
    # Validate that all specified columns are numeric
    if (!all(purrr::map_lgl(df[cols], is.numeric))) stop('All columns must be numeric')
  }

  data.table::setnafill(df, fill = 0, cols = cols)
}


#' Replace numeric NAs with 0
#'
#' This function replaces numeric NAs with 0.
#' @param x list, data.frame, array, or vector
#' @param cols Optional. When appropriate, vector of column names 
#' for which the NA replacement should occur.
#' Default is all numeric columns.
#' @return x with desired NAs replaced with 0.
#' @export
naToZero <- function(x, ...) {
  UseMethod("naToZero")
}

#' @importFrom purrr map_lgl
#' @export
naToZero.list <- function(x, cols = NULL) {

  # if cols not set, use all numeric cols
  if (is.null(cols)) {
    cols <- names(x)[purrr::map_lgl(x, is.numeric)]
  } else {
    # Validate that all specified columns are numeric
    if (!all(purrr::map_lgl(x[cols], is.numeric))) stop('All columns must be numeric')
  }

  x[cols] <- lapply(x[cols], function(y) {y[is.na(y)] <- 0; return(y)})
  return(x)
}

#' @importFrom purrr map_lgl
#' @export
naToZero.data.frame <- function(x, cols = NULL) {

  # if cols not set, use all numeric cols
  if (is.null(cols)) {
    cols <- names(x)[purrr::map_lgl(x, is.numeric)]
  } else {
    # Validate that all specified columns are numeric
    if (!all(purrr::map_lgl(x[cols], is.numeric))) stop('All columns must be numeric')
  }

  x[cols][is.na(x[cols])] <- 0
  return(x)
}


#' @export
naToZero.default <- function(x) {

  numericEntries <- purrr::map_lgl(x, is.numeric)
  x[numericEntries][is.na(x[numericEntries])] <- 0
  return(x)
}