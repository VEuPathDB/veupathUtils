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
