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