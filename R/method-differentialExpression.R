# a helper, to reuse and separate some logic
cleanComparatorVariable <- function(collection, comparator, verbose = c(TRUE, FALSE)) {
  if (!inherits(collection, 'CountDataCollection')) stop("collection must be of the CountDataCollection class.")
  if (!inherits(comparator, 'Comparator')) stop("comparator must be of the Comparator class.")

  comparatorColName <- veupathUtils::getColName(comparator@variable@variableSpec)
  cleanCollection <- removeIncompleteRecords(collection, comparatorColName, verbose)
  data <- getCollectionData(cleanCollection, verbose = verbose)
  sampleMetadata <- getSampleMetadata(cleanCollection)
  recordIdColumn <- cleanCollection@recordIdColumn

  veupathUtils::logWithTime(paste("Received abundance table with", nrow(data), "samples and", (ncol(data)-1), "taxa."), verbose)

  # Subset to only include samples with metadata defined in groupA and groupB
    if (identical(comparator@variable@dataShape@value, "CONTINUOUS")) {
      
      # Ensure bin starts and ends are numeric
      comparator@groupA <- as.numeric(comparator@groupA)
      comparator@groupB <- as.numeric(comparator@groupB)


      # We need to turn the numeric comparison variable into a categorical one with those values
      # that fall within group A or group B bins marked with some string we know.

      # Collect all instances where the comparatorColName has values in the bins from each group.
      # So inGroupA is a vector with 0 if the value in comparatorColName is not within any of the group A bins and >0 otherwise.
      inGroupA <- veupathUtils::whichValuesInBinList(sampleMetadata[[comparatorColName]], comparator@groupA)
      inGroupB <- veupathUtils::whichValuesInBinList(sampleMetadata[[comparatorColName]], comparator@groupB)

      # Eventually move this check to Comparator validation. See #47
      if ((any(inGroupA * inGroupB) > 0)) {
        stop("Group A and Group B cannot have overlapping bins.")
      }

      # Make the comparatorColName a character vector and replace the in-group values with a bin.
      sampleMetadata[, (comparatorColName) := as.character(get(comparatorColName))]
      
      # Now we can reassign groupA and groupB and can replace values in sampleMetadata our new group values
      # We don't care about the values in the comparisonVariable column anymore. They were only
      # useful to help us assign groups.
      sampleMetadata[inGroupA, c(comparatorColName)] <- "groupA"
      sampleMetadata[inGroupB, c(comparatorColName)] <- "groupB"

      # Finally, subset the sampleMetadata to only include those samples in groupA or B
      sampleMetadata <- sampleMetadata[get(comparatorColName) %in% c("groupA", "groupB"), ]

    } else {
      # The comparator must be ordinal, binary, or categorical
      groupAValues <- getGroupLabels(comparator, "groupA")
      groupBValues <- getGroupLabels(comparator, "groupB")

      # Filter sampleMetadata to keep only those samples that are labeled as groupA or groupB. Filter
      # data *before* reassigning values to 'groupA' and 'groupB' to avoid issues with the original variable
      # value being 'groupA' or 'groupB'
      sampleMetadata <- sampleMetadata[get(comparatorColName) %in% c(groupAValues, groupBValues), ]

      # Turn comparatorColName into a binary variable
      sampleMetadata[get(comparatorColName) %in% groupAValues, c(comparatorColName)] <- 'groupA'
      sampleMetadata[get(comparatorColName) %in% groupBValues, c(comparatorColName)] <- 'groupB'
    } 

    # sampleMetadata has already been filtered so it now only contains the samples we care about
    keepSamples <- sampleMetadata[[recordIdColumn]]
    if (!length(keepSamples)) {
      stop("No samples remain after subsetting based on the comparator variable.")
    }
    # need to make sure we actually have two groups
    if (length(unique(sampleMetadata[[comparatorColName]])) < 2) {
      stop("The comparator variable must have at least two values/ groups within the subset.")
    }
    veupathUtils::logWithTime(paste0("Found ",length(keepSamples)," samples with a value for ", comparatorColName, " in either groupA or groupB. The calculation will continue with only these samples."), verbose)

    # Subset the abundance data based on the kept samples
    data <- data[get(recordIdColumn) %in% keepSamples, ]

    cleanCollection@data <- data
    cleanCollection@sampleMetadata <- SampleMetadata(
      data = sampleMetadata,
      recordIdColumn = cleanCollection@sampleMetadata@recordIdColumn
    )
    validObject(cleanCollection)

    return(cleanCollection)
}

#' @export
DifferentialExpressionResult <- setClass("DifferentialExpressionResult", representation(
    effectSizeLabel = 'character',
    statistics = 'data.frame',
    pValueFloor = 'numeric',
    adjustedPValueFloor = 'numeric'
), prototype = prototype(
    effectSizeLabel = 'log2(Fold Change)',
    statistics = data.frame(effectSize = numeric(0),
                            pValue = numeric(0),
                            adjustedPValue = numeric(0),
                            pointID = character(0))
))


setGeneric("deseq",
  function(collection, comparator, verbose = c(TRUE, FALSE)) standardGeneric("deseq"),
  signature = c("collection", "comparator")
)

setMethod("deseq", signature("CountDataCollection", "Comparator"), function(collection, comparator, verbose = c(TRUE, FALSE)) {
  recordIdColumn <- collection@recordIdColumn
  ancestorIdColumns <- collection@ancestorIdColumns
  allIdColumns <- c(recordIdColumn, ancestorIdColumns)
  sampleMetadata <- getSampleMetadata(collection)
  comparatorColName <- veupathUtils::getColName(comparator@variable@variableSpec)

  # First, remove id columns and any columns that are all 0s.
  cleanedData <- purrr::discard(collection@data[, -..allIdColumns], function(col) {identical(union(unique(col), c(0, NA)), c(0, NA))})
  # Next, transpose abundance data to get a counts matrix with taxa as rows and samples as columns
  counts <- data.table::transpose(cleanedData)
  rownames(counts) <- names(cleanedData)
  colnames(counts) <- collection@data[[recordIdColumn]]

  # Then, format metadata. Recall samples are rows and variables are columns
  rownames(sampleMetadata) <- sampleMetadata[[recordIdColumn]]
  
  # Finally, check to ensure samples are in the same order in counts and metadata. DESeq expects this but will not perform the check.
  if (!identical(rownames(sampleMetadata), colnames(counts))){
    # Reorder sampleMetadata to match counts
    veupathUtils::logWithTime("Sample order differs between data and metadata. Reordering data based on the metadata sample order.", verbose)
    data.table::setcolorder(counts, rownames(sampleMetadata))
  }

  deseq_output <- try({

    # Create DESeqDataSet (dds)
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = counts,
                                          colData = sampleMetadata,
                                          design = as.formula(paste0("~",comparatorColName)),
                                          tidy = FALSE)

    # Estimate size factors before running deseq to avoid errors about 0 counts
    geoMeans = apply(DESeq2::counts(dds), 1, function(x){exp(sum(log(x[x > 0]), na.rm=T) / length(x))})
    dds <- DESeq2::estimateSizeFactors(dds, geoMeans = geoMeans)

    # Run DESeq
    deseq_output <- DESeq2::DESeq(dds)
  })

  if (veupathUtils::is.error(deseq_output)) {
    veupathUtils::logWithTime(paste0('Differential abundance FAILED with parameters recordIdColumn=', recordIdColumn, ', method = DESeq', ', verbose =', verbose), verbose)
    stop()
  }

  # Extract deseq results
  deseq_results <- DESeq2::results(deseq_output)

  # Format results for easier access
  statistics <- data.frame(effectSize = deseq_results$log2FoldChange,
                           pValue = deseq_results$pvalue,
                           adjustedPValue = deseq_results$padj,
                           pointID = rownames(counts))

  result <- DifferentialExpressionResult('effectSizeLabel' = 'log2(Fold Change)', 'statistics' = statistics)

  return(result)
})

setMethod("deseq", signature("CollectionWithMetadata", "Comparator"), function(collection, comparator, verbose = c(TRUE, FALSE)) {
  stop("Please use the CountDataCollection class with DESeq2.")
})

#' Differential expression
#'
#' This function returns the fold change and associated p value for a differential expression analysis comparing samples in two groups.
#' 
#' @param collection CollectionWithMetadata object
#' @param comparator Comparator object specifying the variable and values or bins to be used in dividing samples into groups.
#' @param method string defining the the differential abundance method. Accepted values are 'DESeq2' and 'Maaslin2'.
#' @param pValueFloor numeric value that indicates the smallest p value that should be returned. 
#' The corresponding adjusted p value floor will also be updated based on this value, and will be set to the maximum adjusted p value of all floored p values.
#' The default value uses the P_VALUE_FLOOR=1e-200 constant defined in this package.
#' @param verbose boolean indicating if timed logging is desired
#' @return ComputeResult object
#' @import data.table
#' @import DESeq2
#' @importFrom purrr none
#' @importFrom purrr discard
#' @export
setGeneric("differentialExpression",
  function(collection, comparator, method = c('DESeq'), pValueFloor = P_VALUE_FLOOR, verbose = c(TRUE, FALSE)) standardGeneric("differentialExpression"),
  signature = c("collection", "comparator")
)

# This main function stays consistent regardless of the type of data we give to it. For example, in case we add non-counts data in the future.
#'@export
setMethod("differentialExpression", signature("CollectionWithMetadata", "Comparator"), function(collection, comparator, method = c('DESeq'), pValueFloor = P_VALUE_FLOOR, verbose = c(TRUE, FALSE)) {
    cleanCollection <- cleanComparatorVariable(collection, comparator, verbose)
    recordIdColumn <- cleanCollection@recordIdColumn
    ancestorIdColumns <- cleanCollection@ancestorIdColumns
    allIdColumns <- c(recordIdColumn, ancestorIdColumns)
    comparatorColName <- veupathUtils::getColName(comparator@variable@variableSpec)

    ## Initialize and check inputs
    method <- veupathUtils::matchArg(method)
    verbose <- veupathUtils::matchArg(verbose)

    
    ## Compute differential abundance
    if (identical(method, 'DESeq')) {
      statistics <- deseq(cleanCollection, comparator, verbose)
    } else {
      stop('Unaccepted differential abundance method. Accepted methods are "DESeq".')
    }
    veupathUtils::logWithTime(paste0('Completed method=',method,'. Formatting results.'), verbose)

    # Sometimes p-values can be very small, even smaller than the smallest representable number (gives p-value=0). The smallest
    # representable number changes based on env, so to avoid inconsistency set a p-value floor so that any
    # returned p-value less than the floor becomes the floor. 
    # The default floor is a constant defined in the microbiomeComputations package.

    # First find indices of the small p-values and update these values to be the pValueFloor
    smallPValueIndices <- which(statistics@statistics[["pValue"]] < pValueFloor)
    if (length(smallPValueIndices) > 0) {
      statistics@statistics[smallPValueIndices, "pValue"] <- pValueFloor

      # Second, find the adjusted p value floor by taking the largest adjusted p-value of those p-values that were floored
      smallAdjPValues <- statistics@statistics[smallPValueIndices, "adjustedPValue"]
      adjustedPValueFloor <- max(smallAdjPValues)

      # Finally, update the adjusted p-values with the floor
      statistics@statistics[smallPValueIndices, "adjustedPValue"] <- adjustedPValueFloor
    } else {
      adjustedPValueFloor <- NA_real_
    }
    statistics@pValueFloor <- pValueFloor
    statistics@adjustedPValueFloor <- adjustedPValueFloor

    
    # Record columns that were dropped due to data cleaning.
    droppedColumns <- setdiff(names(cleanCollection@data[, -..allIdColumns, with=FALSE]), statistics@statistics$pointID)

    ## Construct the ComputeResult
    result <- new("ComputeResult")
    result@name <- 'differentialExpression'
    result@recordIdColumn <- recordIdColumn
    result@ancestorIdColumns <- ancestorIdColumns
    result@statistics <- statistics
    result@parameters <- paste0('recordIdColumn = ', recordIdColumn,", comparatorColName = ", comparatorColName, ', method = ', method, ', groupA =', getGroupLabels(comparator, "groupA"), ', groupB = ', getGroupLabels(comparator, "groupB"))
    result@droppedColumns <- droppedColumns


    # The resulting data should contain only the samples actually used.
    result@data <- cleanCollection@data[, ..allIdColumns]
    names(result@data) <- veupathUtils::stripEntityIdFromColumnHeader(names(result@data))


    validObject(result)
    veupathUtils::logWithTime(paste('Differential expression computation completed with parameters recordIdColumn = ', recordIdColumn,", comparatorColName = ", comparatorColName, ', method = ', method, ', groupA =', getGroupLabels(comparator, "groupA"), ', groupB = ', getGroupLabels(comparator, "groupB")), verbose)
    
    return(result)
})
