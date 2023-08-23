

#' Find values that fall within a Bin
#' 
#' Given a numeric vector and a Bin, returns a logical vector indicating if 
#' the value in the input vector falls within the bin.
#' @param values numeric vector
#' @param bin A Bin object
#' @return logical vector
#' @export
setGeneric("whichValuesInBin", 
  function(values, bin) standardGeneric("whichValuesInBin"),
  signature = "bin"
)

#' @export
setMethod("whichValuesInBin", signature("Bin"), function(values, bin) {

  inBin <- (values >= bin@binStart) * (values < bin@binEnd)
  return (as.logical(inBin))

})


#' Find values in a vector that fall within any bins in a BinList
#' 
#' Given a vector of values and a BinList, return a logical vector that 
#' indicates if a value was found in any Bin within the BinList
#' 
#' @param values numeric vector
#' @param binList A BinList object
#' @return logical vector
#' @export 
setGeneric("whichValuesInBinList", 
  function(values, binList) standardGeneric("whichValuesInBinList"),
  signature = "binList"
)

#' @export
setMethod("whichValuesInBinList", signature("BinList"), function(values, binList) {

  inBin <- Reduce(`+`, lapply(binList, whichValuesInBin, values = values))
  return (as.logical(inBin))

})