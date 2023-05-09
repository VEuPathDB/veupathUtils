#' Find Bin Ranges for a Continuous Variable
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
getDiscretizedBins <- function(x, method = c('equalInterval', 'quantile', 'sd'), numBins = NULL, getValue = c(TRUE, FALSE)) {
  method <- veupathUtils::matchArg(method)
  getValue <- veupathUtils::matchArg(getValue)
  if (is.null(numBins)) numBins <- 10

  isDate <- FALSE
  if (class(x) == 'Date') {
    x <- as.numeric(x)
    isDate <- TRUE
  }

  binEdges <- unname(breaks(x, method, numBins))
  if (anyDuplicated(binEdges)) {
    warning("There is insufficient data to produce the requested number of bins. Returning as many bins as possible.")
    binEdges <- unique(binEdges)
  }

  if (isDate) {
    binEdges <- as.Date(binEdges, origin = "1970-01-01")
  }

  binStarts <- binEdges[1:(length(binEdges)-1)]
  binEnds <- binEdges[2:length(binEdges)]
  if (length(binEdges) == 1) binEnds <- binEnds[[2]]

  # only format human-friendly labels. binStarts and binEnds should provide exact values
  # must also guarantee that the first binStart and last binEnd encompass the full data range even after formatting
  if (isDate) {
    # Dates are already human-friendly.
    formattedBinStarts <- binStarts
    formattedBinEnds <- binEnds
  } else {
    formattedBinStarts <- formatC(binStarts)
    formattedBinEnds <- formatC(binEnds)
  }
  # think the alternative is to write a recursive fxn to call formatC w more digits until we get a result we like. 
  # that seems costly, so ill wait to do that until we see how much an issue this really is
  if (as.numeric(formattedBinStarts[[1]]) > as.numeric(binStarts[[1]])) formattedBinStarts[[1]] <- as.character(binStarts[[1]])
  if (as.numeric(formattedBinEnds[[length(binEnds)]]) < as.numeric(binEnds[[length(binEnds)]])) formattedBinEnds[[length(binEnds)]] <- as.character(binEnds[[length(binEnds)]])

  binLabels <- paste0("(",formattedBinStarts,", ", formattedBinEnds, "]")
  binLabels[[1]] <- gsub("(","[",binLabels[[1]], fixed=T)

  if (getValue) {
    if (length(binEdges) == 1) {
      values <- 1
    } else {
      values <- c(table(cut(x, binEdges, include.lowest=TRUE)))
    }
  } else {
    values <- rep(NA_real_, length(binStarts))
  }
  
  bins <- lapply(1:length(binStarts), FUN = function(x) { Bin(binStart = binStarts[[x]],
                                                              binEnd = binEnds[[x]],
                                                              binLabel = binLabels[[x]],
                                                              value = values[[x]])})

  return(BinList(S4Vectors::SimpleList(bins)))
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

#
# For any number, return an absolute delta (numeric) at the last
# significant digit in the number, using the number of digits specified
#
# e.g. assuming 3 significant digits
# 
# 1.23 -> 0.01
# 11.0 -> 0.1
# 12.3 -> 0.1
# 101000 -> 1000
# 1.20e-05 -> 0.01e-05 == 1.0e-07
# 0.0123e-05 -> 0.0001e-05 == 1.0e-09
# -2.34e-02 -> 0.01e-02 == 1.0e-04
# 
signifDigitEpsilon <- function(x, digits) {

  # '#' flag ensures trailing zeroes
  # take abs() here because we don't care about sign
  rounded <- formatC(abs(x), digits = digits, width = 1L, flag = '#')

  # split into vector of single characters
  characters <- strsplit(rounded, '')

  result <- c()
  seenSignificant <- FALSE
  significantCount <- 0
  # walk through string, looking for first non-zero, non decimal point character
  for (c in unlist(characters)) {
    if (!(c %in% c('0', '.'))) {
      seenSignificant <- TRUE
    }
    if (c == '.') {
      result <- c(result, c)
    } else if (seenSignificant) {
      significantCount <- significantCount + 1
      if (significantCount < digits) {
        result <- c(result, '0')
      } else if (significantCount == digits) {
        result <- c(result, '1')
      } else {
        # we're out of the significant digits
        # we must be in the exponent part (if present) or in trailing zeroes (e.g. in 101000 example)
        # so just copy it over
        result <- c(result, c)
      }
    } else {
      result <- c(result, '0')
    }
  }

  # return joined result as a number
  as.numeric(paste(result, collapse=""))
}
