
#' PCA
#' 
#' @param collection A Collection object
#' @param nPCs Number of principal components to return. Default 10
#' @param ntop Use the top ntop genes with the highest variance for the pca computation. Mirrors the deseq2 plotPCA argument. Default 500.
#' @param verbose Boolean indicating if extra messaging should be printed.
#' @return A data table with the id columns and the first nPCs principal components.
#' @export
setGeneric("pca",
  function(collection, nPCs = 10, ntop = 500, verbose = c(TRUE, FALSE)) standardGeneric("pca"),
  signature = c("collection")
)

#' @export
setMethod(pca, "Collection",
  function(collection, nPCs = 10, ntop = 500, verbose = c(TRUE, FALSE)) {
    
    verbose <- veupathUtils::matchArg(verbose)
    assay <- getCollectionData(collection)
    recordIdColumn <- collection@recordIdColumn
    ancestorIdColumns <- collection@ancestorIdColumns
    allIdColumns <- c(recordIdColumn, ancestorIdColumns)
    
    # Remove id columns from the assay to get only the features.
    features <- assay[, -..allIdColumns] # features has samples as rows.

    # Update ntop if it's too large.
    if (ntop > ncol(features)) {
      if (verbose) {
        message("ntop is larger than the number of features. Using all features.")
      }
      ntop <- min(ntop, ncol(features))
    }

    # Ensure ntop is at least 1.
    if (ntop <= 1) {
      stop("ntop must be at least 2.")
    }

    # Use prcomp to perform PCA. 
    # The following is heavily borrowed from the deseq2 plotPCA function.
    rowVariances <- matrixStats::rowVars(t(as.matrix(features)))
    keepFeatures <- order(rowVariances, decreasing=TRUE)[seq_len(ntop)]
    pcaResult <- prcomp(features[, ..keepFeatures])


    # Assemble output data
    dt <- assay[, ..allIdColumns]
    # The PCA results are in pcaResult$x. Keep the first nPCs PCS.
    dt <- cbind(dt, pcaResult$x[, 1:nPCs]) # this works fine even with one id column

    return(dt)
  }
)