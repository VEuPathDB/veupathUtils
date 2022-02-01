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


#' Impute 0s for numeric columns
#'
#' This function replaces NAs in numeric columns of a data.table with 0.
#' @param dt data.table
#' @param cols Vector of column names for which the NA replacement should occur.
#' Default is all numeric columns.
#' @return inputted data.table dt but with NAs replaced with desired value.
#' @export
setNaToZero <- function(dt, cols = NULL) {

  # if cols not set, use all numeric cols
  if (is.null(cols)) {
    cols <- colnames(dt)[sapply(dt, class) == "numeric"]
  }
  
  # Ensure data.table
  if (!'data.table' %in% class(dt)) {
    data.table::setDT(dt)
  }

  data.table::setnafill(dt, fill = 0, cols = cols)

}