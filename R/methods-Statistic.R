

#' Convert StatisicList to data.table
#' 
#' This function returns a data.table representation of a StatisticList.
#' The content of the slots are stored as JSON. Each entry in the list becomes a named column.
#' @param object A StatisticList object
#' @return data.table
#' @export
setGeneric("as.data.table", 
  function(object) standardGeneric("as.data.table"),
  signature = "object"
)

#' @export
setMethod("as.data.table", signature("StatisticList"), function(object) {
    colNames <- unlist(lapply(as.list(object), function(x) {x@name}))
    dt <- data.table::as.data.table(as.list(object))
    data.table::setnames(dt, colNames)

    return(dt)
})

#' @export
setMethod("as.data.table", signature("Statistic"), function(object) {
    colName <- object@name
    dt <- data.table::as.data.table(colName = object)

    return(dt)
})