#' Manage Collection Names
#' 
#' Get or set the name of the collection
#' @param x A Collection
#' @rdname name
#' @export
setGeneric("name", function(x) standardGeneric("name"))

#' @rdname name
#' @aliases name,Collection-method
setMethod("name", "Collection", function(x) return(x@name))

#' @rdname name
#' @param value The new name of the collection
#' @export
setGeneric("name<-", function(x, value) standardGeneric("name<-"))

#' @rdname name
#' @aliases name<-,Collection,character-method
setMethod("name<-", "Collection", function(x, value) {x@name <- value; return(x)})

#' Get Names of Collections
#' 
#' Get the names of the collections in the Collections or MbioDataset object
#' @param object A Collections object, or MbioDataset
#' @return A character vector of collection names
#' @export
#' @rdname getCollectionNames
setGeneric("getCollectionNames", function(object) standardGeneric("getCollectionNames"))

#' @rdname getCollectionNames
#' @aliases getCollectionNames,Collections-method
setMethod("getCollectionNames", "Collections", function(object) return(sapply(object, name)))