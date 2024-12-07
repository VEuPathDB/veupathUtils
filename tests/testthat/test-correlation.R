test_that('correlation works with two data tables', {
  nSamples = 200

  testData1 <- data.table(
    "contA1" = rnorm(nSamples),
    "contB1" = rnorm(nSamples),
    "contC1" = rnorm(nSamples)
  )

  testData2 <- data.table(
    "contA2" = rnorm(nSamples),
    "contB2" = rnorm(nSamples),
    "contC2" = rnorm(nSamples)
  )

  corrResult <- correlation(testData1, testData2, method = 'spearman', format = 'data.table', verbose=F)
  expect_equal(names(corrResult), c("data1","data2","correlationCoef","pValue"))
  expect_equal(nrow(corrResult), ncol(testData1) * ncol(testData2))
  expect_true(!all(is.na(corrResult$correlationCoef)))

  corrResult <- correlation(testData1, testData2, method = 'pearson', format = 'data.table', verbose=F)
  expect_equal(names(corrResult), c("data1","data2","correlationCoef","pValue"))
  expect_equal(nrow(corrResult), ncol(testData1) * ncol(testData2))
  expect_true(!all(is.na(corrResult$correlationCoef)))

  # Test with NAs
  # Should compute using complete cases. 
  # This to try to catch regression back to the default behavior of cor, which returns results of NA if any value is NA
  testData2$contA2[1] <- NA
  corrResult <- correlation(testData1, testData2, method = 'pearson', format = 'data.table', verbose=F)
  expect_equal(names(corrResult), c("data1","data2","correlationCoef","pValue"))
  expect_equal(nrow(corrResult), ncol(testData1) * ncol(testData2))
  expect_true(!all(is.na(corrResult$correlationCoef)))

})

test_that("correlation works with a single data table", {
  nSamples = 200
  testData1 <- data.table(
    "contA1" = rnorm(nSamples),
    "contB1" = rnorm(nSamples),
    "contC1" = rnorm(nSamples)
  )
  corrResult <- correlation(testData1, method='spearman', format = 'data.table', verbose=F)
  expect_equal(names(corrResult), c("data1","data2","correlationCoef","pValue"))
  expect_equal(nrow(corrResult), (ncol(testData1) * ncol(testData1) - 3)/2)
  expect_true(!all(is.na(corrResult$correlationCoef)))

  corrResult <- correlation(testData1, method='pearson', format = 'data.table', verbose=F)
  expect_equal(names(corrResult), c("data1","data2","correlationCoef","pValue"))
  expect_equal(nrow(corrResult), (ncol(testData1) * ncol(testData1) - 3)/2)
  expect_true(!all(is.na(corrResult$correlationCoef)))

  corrResult <- correlation(abs(testData1), method='sparcc', format = 'data.table', verbose=F) #sparcc wont like negative values
  expect_equal(names(corrResult), c("data1","data2","correlationCoef","pValue"))
  expect_equal(nrow(corrResult), (ncol(testData1) * ncol(testData1) - 3)/2)
  expect_true(!all(is.na(corrResult$correlationCoef)))

  #test alias
  corrResult <- selfCorrelation(testData1, method='pearson', format = 'data.table', verbose=F) 
  expect_equal(names(corrResult), c("data1","data2","correlationCoef","pValue"))
  expect_equal(nrow(corrResult), (ncol(testData1) * ncol(testData1) - 3)/2)
  expect_true(!all(is.na(corrResult$correlationCoef)))

  # Test with NAs
  # Should compute using complete cases. 
  # This to try to catch regression back to the default behavior of cor, which returns results of NA if any value is NA
  testData1$contA1[1] <- NA
  corrResult <- correlation(testData1, method='pearson', format = 'data.table', verbose=F)
  expect_equal(names(corrResult), c("data1","data2","correlationCoef","pValue"))
  expect_equal(nrow(corrResult), (ncol(testData1) * ncol(testData1) - 3)/2)
  expect_true(!all(is.na(corrResult$correlationCoef)))

})

test_that("toJSON works as expected for the CorrelationResult class", {
  
  nSamples <- 200
  sampleMetadata <- data.table::data.table(
      "entity.SampleID" = 1:nSamples,
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
  )

  data <- data.table::data.table(
    "entity.SampleID" = sampleMetadata$entity.SampleID,
    "entity.cont1" = rnorm(nSamples),
    "entity.cont2" = rnorm(nSamples),
    "entity.cont3" = rnorm(nSamples)
  )

  result <- correlation(data, sampleMetadata, method='pearson', verbose = FALSE)
  jsonList <- jsonlite::fromJSON(toJSON(result@statistics))
  expect_equal(names(jsonList), c('data1Metadata', 'data2Metadata', 'statistics'))
  expect_equal(class(jsonList$data1Metadata), "character")
  expect_equal(class(jsonList$data2Metadata), "character")
  expect_equal(names(jsonList$statistics), c('data1', 'data2', 'correlationCoef', 'pValue'))
  expect_equal(unname(unlist(lapply(jsonList$statistics, class))), c('character', 'character', 'character', 'character'))
})

# Tests for CollectionWithMetadata specific correlation methods

test_that('correlation returns an appropriately structured result for abundance data vs metadata', {
  
  nSamples <- 200
  sampleMetadataDT <- data.table::data.table(
      "entity.SampleID" = 1:nSamples,
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
  )

  df <- data.table::data.table(
    "entity.SampleID" = sampleMetadataDT$entity.SampleID,
    "entity.cont1" = rnorm(nSamples),
    "entity.cont2" = rnorm(nSamples),
    "entity.cont3" = rnorm(nSamples)
  )

  sampleMetadata <- SampleMetadata(
    data = sampleMetadataDT,
    recordIdColumn = "entity.SampleID"
  )

  data <- CollectionWithMetadata(
              name = 'testing',
              data = df,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')
  
  ## All numeric sample variables
  result <- correlation(data, method='pearson', proportionNonZeroThreshold = 0, verbose = FALSE)
  expect_equal(result@statistics@data1Metadata, 'assay')
  expect_equal(result@statistics@data2Metadata, 'sampleMetadata')
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 9) # Should be number of collection members * number of metadata vars
  expect_true(all(!is.na(statsData)))
  # if metadataIsFirst is true, data1 and data2 are switched
  result <- correlation(data, method='pearson', proportionNonZeroThreshold = 0, verbose = FALSE, metadataIsFirst = TRUE)
  expect_equal(result@statistics@data1Metadata, 'sampleMetadata')
  expect_equal(result@statistics@data2Metadata, 'assay')


  ## With method = spearman
  result <- correlation(data, method='spearman', proportionNonZeroThreshold = 0, verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 9) # Should be number of collection members * number of metadata vars
  expect_true(all(!is.na(statsData)))


  # with a single metadata var
  sampleMetadata <- SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.contA" = rnorm(nSamples)
      )),
    recordIdColumn = "entity.SampleID"
  )

  data <- CollectionWithMetadata(
              name = 'testing',
              data = df,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  result <- correlation(data, method='spearman', proportionNonZeroThreshold = 0, verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 3) # Should be number of collection members * number of metadata vars
  expect_true(all(!is.na(statsData)))
})


test_that("correlation returns an appropriately structured result for metadata vs metadata", {
  
  nSamples <- 200
  sampleMetadataDT <- data.table::data.table(
      "entity.SampleID" = 1:nSamples,
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
  )

  sampleMetadata <- SampleMetadata(
    data = sampleMetadataDT,
    recordIdColumn = "entity.SampleID"
  )             

  result <- selfCorrelation(sampleMetadata, method='pearson', verbose = FALSE)
  expect_equal(result@statistics@data1Metadata, 'sampleMetadata')
  expect_equal(result@statistics@data2Metadata, 'sampleMetadata')

  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  nNumericCols <- length(veupathUtils::findNumericCols(sampleMetadata@data[,2:ncol(sampleMetadata@data)]))
  expect_equal(nrow(statsData), ((nNumericCols * nNumericCols) - 3)/2) # Should be number of number of numeric vars * number of numeric vars
  expect_equal(as.character(unique(statsData$data1)), c('entity.contA', 'entity.contB'))
  expect_equal(as.character(unique(statsData$data2)), c('entity.contB', 'entity.contC'))
  expect_true(all(!is.na(statsData)))

  ## With method = spearman
  result <- selfCorrelation(sampleMetadata, method='spearman', verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), ((nNumericCols * nNumericCols) - 3)/2) # Should be number of collection variables * number of metadata vars
  expect_equal(as.character(unique(statsData$data1)), c('entity.contA', 'entity.contB'))
  expect_equal(as.character(unique(statsData$data2)), c('entity.contB', 'entity.contC'))
  expect_true(all(!is.na(statsData)))

})

test_that("correlation returns an appropriately structured result for assay against self", {

  nSamples <- 200
  sampleMetadataDT <- data.table::data.table(
      "entity.SampleID" = 1:nSamples,
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
  )

  df <- data.table::data.table(
    "entity.SampleID" = sampleMetadataDT$entity.SampleID,
    "entity.cont1" = rnorm(nSamples),
    "entity.cont2" = rnorm(nSamples),
    "entity.cont3" = rnorm(nSamples)
  )

  sampleMetadata <- SampleMetadata(
    data = sampleMetadataDT,
    recordIdColumn = "entity.SampleID"
  )

  data <- CollectionWithMetadata(
              name = 'testing',
              data = df,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  result <- selfCorrelation(data, method='pearson', proportionNonZeroThreshold = 0, verbose = FALSE)
  expect_equal(result@statistics@data1Metadata, 'assay')
  expect_equal(result@statistics@data2Metadata, 'assay')

  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 3) # ((3*3)-3)/2
  expect_equal(as.character(unique(statsData$data1)), names(df)[2:(length(names(df))-1)])
  expect_equal(as.character(unique(statsData$data2)), names(df)[3:length(names(df))])
  expect_true(all(!is.na(statsData)))

  # method = spearman
  result <- selfCorrelation(data, method='spearman', proportionNonZeroThreshold = 0, verbose = FALSE)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 3) # ((3*3)-3)/2
  expect_equal(as.character(unique(statsData$data1)), names(df)[2:(length(names(df)) - 1)])
  expect_equal(as.character(unique(statsData$data2)), names(df)[3:length(names(df))])
  expect_true(all(!is.na(statsData)))
})

test_that("correlation returns an appropriately structured result for assay vs assay", {

  nSamples <- 200
  df1 <- data.table::data.table(
      "entity.SampleID" = 1:nSamples,
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
  )

  df2 <- data.table::data.table(
    "entity.SampleID" = df1$entity.SampleID,
    "entity.cont1" = rnorm(nSamples),
    "entity.cont2" = rnorm(nSamples),
    "entity.cont3" = rnorm(nSamples)
  )

  sampleMetadata <- SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df1[["entity.SampleID"]],
      "entity.contX" = rnorm(nSamples),
      "entity.contY" = rnorm(nSamples),
      "entity.contZ" = rnorm(nSamples)
      )),
    recordIdColumn = "entity.SampleID"
  )

  data1 <- CollectionWithMetadata(
              name = 'testing',
              data = df1,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  data2 <- CollectionWithMetadata(
              name = 'testing2',
              data = df2,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')
  
  ## All numeric sample variables
  result <- correlation(data1, data2, method='pearson', proportionNonZeroThreshold = 0, verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 9) # Should be number of variables in df1 * number of variables in df2
  expect_true(all(!is.na(statsData)))


  ## With method = spearman
  result <- correlation(data1, data2, method='spearman', proportionNonZeroThreshold = 0, verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 9) # Should be number of variables in df1 * number of variables in df2
})

test_that("correlation returns a ComputeResult with the correct slots", {

  nSamples <- 200
  sampleMetadataDT <- data.table::data.table(
      "entity.SampleID" = 1:nSamples,
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
  )

  df <- data.table::data.table(
    "entity.SampleID" = sampleMetadataDT$entity.SampleID,
    "entity.cont1" = rnorm(nSamples),
    "entity.cont2" = rnorm(nSamples),
    "entity.cont3" = rnorm(nSamples)
  )

  sampleMetadata <- SampleMetadata(
    data = sampleMetadataDT,
    recordIdColumn = "entity.SampleID"
  )

  data <- CollectionWithMetadata(
              name = 'testing',
              data = df,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  ## Pearson
  result <- correlation(data, method='pearson', verbose = FALSE)
  expect_equal(result@parameters, 'method = pearson')
  expect_equal(result@recordIdColumn, 'entity.SampleID')
  expect_equal(result@statistics@data1Metadata, 'assay')
  expect_equal(result@statistics@data2Metadata, 'sampleMetadata')

  ## With spearman
  result <- correlation(data, method='spearman', verbose = FALSE)
  expect_equal(result@parameters, 'method = spearman')
  expect_equal(result@recordIdColumn, 'entity.SampleID')
  expect_equal(result@statistics@data1Metadata, 'assay')
  expect_equal(result@statistics@data2Metadata, 'sampleMetadata')

})

test_that("correlation fails with improper inputs", {

  nSamples <- 200
  df <- data.table::data.table(
    "entity.SampleID" = 1:nSamples,
    "entity.cont1" = rnorm(nSamples),
    "entity.cont2" = rnorm(nSamples),
    "entity.cont3" = rnorm(nSamples)
  )
  
  counts <- round(df[, -c("entity.SampleID")]*1000) # make into "counts"
  counts[ ,entity.SampleID:= df$entity.SampleID]

  sampleMetadata <- SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.binA" = sample(c("binA_a", "binA_b"), nSamples, replace=T),
      "entity.cat2" = sample(c("cat2_a", "cat2_b"), nSamples, replace=T),
      "entity.cat3" = sample(paste0("cat3_", letters[1:3]), nSamples, replace=T),
      "entity.cat4" = sample(paste0("cat4_", letters[1:4]), nSamples, replace=T)
      )),
    recordIdColumn = "entity.SampleID"
  )


  data <- CollectionWithMetadata(
              name = 'testing',
              data = counts,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  data@sampleMetadata <- sampleMetadata

  # Fail when we send in only categorical metadata
  expect_error(correlation(data, verbose=F))

  sampleMetadataDT <- data.table::data.table(
      "entity.SampleID" = 1:nSamples,
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
  )

  sampleMetadata <- SampleMetadata(
    data = sampleMetadataDT,
    recordIdColumn = "entity.SampleID"
  )

  # Fail when sample metadata is missing a sample
  sampleMetadata@data <- sampleMetadata@data[-1, ]
  expect_error(corrleation(data, verbose=F))
})

test_that("correlation succeeds w a mix of cat and cont metadata", {
  nSamples <- 200
  df <- data.table::data.table(
    "entity.SampleID" = 1:nSamples,
    "entity.cont1" = rnorm(nSamples),
    "entity.cont2" = rnorm(nSamples),
    "entity.cont3" = rnorm(nSamples)
  )
  
  counts <- round(df[, -c("entity.SampleID")]*1000) # make into "counts"
  counts[ ,entity.SampleID:= df$entity.SampleID]

  sampleMetadata <- SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.binA" = sample(c("binA_a", "binA_b"), nSamples, replace=T),
      "entity.cat2" = sample(c("cat2_a", "cat2_b"), nSamples, replace=T),
      "entity.cat3" = sample(paste0("cat3_", letters[1:3]), nSamples, replace=T),
      "entity.cat4" = sample(paste0("cat4_", letters[1:4]), nSamples, replace=T),
      "entity.cont1" = rnorm(nSamples),
      "entity.cont2" = rnorm(nSamples),
      "entity.cont3" = rnorm(nSamples)
      )),
    recordIdColumn = "entity.SampleID"
  )

  data <- CollectionWithMetadata(
              name = 'testing',
              data = counts,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  data@sampleMetadata <- sampleMetadata

  result <- correlation(data, method='pearson', proportionNonZeroThreshold = 0, verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 9) # Should be number of variables in df1 * number of variables in df2
  expect_true(all(!is.na(statsData)))
})

test_that("toJSON works as expected for the CorrelationResult class", {

  nSamples <- 200 
  sampleMetadataDT <- data.table::data.table(
      "entity.SampleID" = 1:nSamples,
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
  )

  df <- data.table::data.table(
    "entity.SampleID" = sampleMetadataDT$entity.SampleID,
    "entity.cont1" = rnorm(nSamples),
    "entity.cont2" = rnorm(nSamples),
    "entity.cont3" = rnorm(nSamples)
  )

  sampleMetadata <- SampleMetadata(
    data = sampleMetadataDT,
    recordIdColumn = "entity.SampleID"
  )

  data <- CollectionWithMetadata(
              name = 'testing',
              data = df,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  result <- correlation(data, method='pearson', verbose = FALSE)
  jsonList <- jsonlite::fromJSON(toJSON(result@statistics))
  expect_equal(names(jsonList), c('data1Metadata', 'data2Metadata', 'statistics'))
  expect_equal(class(jsonList$data1Metadata), "character")
  expect_equal(class(jsonList$data2Metadata), "character")
  expect_equal(names(jsonList$statistics), c('data1', 'data2', 'correlationCoef', 'pValue'))
  expect_equal(unname(unlist(lapply(jsonList$statistics, class))), c('character', 'character', 'character', 'character'))
})