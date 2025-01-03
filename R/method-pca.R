
#' PCA
#' 
#' @param collection A Collection object
#' @param nPCs Number of principal components to return. Default 10
#' @param ntop Use the top ntop genes with the highest variance for the pca computation. Mirrors the deseq2 plotPCA argument. Default 500.
#' @param verbose Boolean indicating if extra messaging should be printed.
#' @return A data frame with the first two principal components
#' @export
setGeneric("pca",
  function(collection, nPCs = 10, ntop = 500, verbose = c(TRUE, FALSE)) standardGeneric("pca"),
  signature = c("collection")
)

#' @export
setMethod(pca, "Collection",
  function(collection, nPCs = 10, ntop = 500, verbose = c(TRUE, FALSE)) {
    
    # Get the assay data
    assay <- getCollectionData(collection)
    recordIdColumn <- collection@recordIdColumn
    ancestorIdColumns <- collection@ancestorIdColumns
    allIdColumns <- c(recordIdColumn, ancestorIdColumns)
    # remove id columns
    features <- assay[, -..allIdColumns]
    # Currently taxa/genes are columns and samples are rows

    # From deseq plotpca, 
    rv <- matrixStats::rowVars(t(as.matrix(features)))
    # Now we have a vector of variances for each gene
    select <- order(rv, decreasing=TRUE)[seq_len(min(ntop, length(rv)))]
    pca <- prcomp(features[, ..select])


    # assemble data frame
    dt <- assay[, ..allIdColumns]
    # combine with the first 20 columns of pca$x
    dt <- cbind(dt, pca$x[, 1:20]) # this works fine even with one id column

    # Then we send to plot.data?
    # have to merge with overlay variable
    return(dt)
  }
)

# assay <- getCollectionData(testData)
# recordIdColumn <- testData@recordIdColumn
# ancestorIdColumns <- testData@ancestorIdColumns
# allIdColumns <- c(recordIdColumn, ancestorIdColumns)
# # remove id columns
# features <- assay[, -..allIdColumns]
# # Currently taxa/genes are columns and samples are rows

# # From deseq plotpca, 
# rv <- matrixStats::rowVars(t(as.matrix(features)))
# # Now we have a vector of variances for each gene
# ntop = 500 # Default value. Should we make this a variable?
# select <- order(rv, decreasing=TRUE)[seq_len(min(ntop, length(rv)))]
# pca <- prcomp(features[, ..select])

# # The good stuff is in pca$x
# x <- pca$x[, 1]
# y <- pca$x[, 2]

# # assemble data frame
# dt <- assay[, ..allIdColumns]
# # combine with the first 20 columns of pca$x
# dt <- cbind(dt, pca$x[, 1:20]) # this works fine even with one id column

# # Then we send to plot.data?
# # have to merge with overlay variable

