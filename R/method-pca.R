# PCA

# Args: ntop, data, nPCs, 
# test with testCountDataCollection
testData <- testCountDataCollection


assay <- getCollectionData(testData)
recordIdColumn <- testData@recordIdColumn
ancestorIdColumns <- testData@ancestorIdColumns
allIdColumns <- c(recordIdColumn, ancestorIdColumns)
# remove id columns
features <- assay[, -..allIdColumns]
# Currently taxa/genes are columns and samples are rows

# From deseq plotpca, 
rv <- matrixStats::rowVars(t(as.matrix(features)))
# Now we have a vector of variances for each gene
ntop = 500 # Default value. Should we make this a variable?
select <- order(rv, decreasing=TRUE)[seq_len(min(ntop, length(rv)))]
pca <- prcomp(features[, ..select])

# The good stuff is in pca$x
x <- pca$x[, 1]
y <- pca$x[, 2]

# assemble data frame
dt <- assay[, ..allIdColumns]
# combine with the first 20 columns of pca$x
dt <- cbind(dt, pca$x[, 1:20]) # this works fine even with one id column

# Then we send to plot.data?
# have to merge with overlay variable

