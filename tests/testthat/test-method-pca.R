# Test PCA

test_that("The pca function produces the expected output", {
  
  testData <- testCountDataCollection
  
  # Run pca
  load_all()
  output <- veupathUtils::pca(testData, nPCs = 2)
  expect_s4_class(output, "ComputeResult")
  outputData <- output@data
  expect_s3_class(outputData, "data.table")
  expect_equal(nrow(outputData), nrow(getCollectionData(testData)))
  expect_equal(ncol(outputData), 3)
  expect_equal(names(outputData), c("entity.SampleID", "PC1", "PC2"))
  computedVariableMetadata <- output@computedVariableMetadata
  expect_s4_class(computedVariableMetadata, "VariableMetadataList")
  expect_equal(length(computedVariableMetadata), 2)
  expect_s4_class(computedVariableMetadata[[1]], "VariableMetadata")
  expect_s4_class(computedVariableMetadata[[2]], "VariableMetadata")
  expect_equal(computedVariableMetadata[[1]]@variableSpec@variableId, "PC1")
  expect_equal(computedVariableMetadata[[2]]@variableSpec@variableId, "PC2")

  
  # Generate some fake data
  nSamples <- 500
  fakeData <- data.table(
    entity.SampleID = paste0("Sample", 1:nSamples),
    feat1 = rnorm(nSamples),
    feat2 = rnorm(nSamples),
    feat3 = rnorm(nSamples)
  )

  fakeCollection <- new("Collection", data = fakeData, recordIdColumn = "entity.SampleID", name="test")

  output <- veupathUtils::pca(fakeCollection, nPCs = 2, ntop=20)
  expect_s4_class(output, "ComputeResult")
  outputData <- output@data
  expect_s3_class(outputData, "data.table")
  expect_equal(nrow(outputData), nSamples)
  expect_equal(ncol(outputData), 3)
  expect_equal(names(outputData), c("entity.SampleID", "PC1", "PC2"))
  computedVariableMetadata <- output@computedVariableMetadata
  expect_s4_class(computedVariableMetadata, "VariableMetadataList")
  expect_equal(length(computedVariableMetadata), 2)
  expect_s4_class(computedVariableMetadata[[1]], "VariableMetadata")
  expect_s4_class(computedVariableMetadata[[2]], "VariableMetadata")
  expect_equal(computedVariableMetadata[[1]]@variableSpec@variableId, "PC1")
  expect_equal(computedVariableMetadata[[2]]@variableSpec@variableId, "PC2")
})

test_that("The pca function can handle messy data", {

  # Generate some fake data
  nSamples <- 500
  fakeData <- data.table(
    entity.SampleID = paste0("Sample", 1:nSamples),
    entity.feat1 = rnorm(nSamples),
    entity.feat2 = rnorm(nSamples),
    entity.feat3 = rnorm(nSamples)
  )

  fakeCollection <- new("Collection", data = fakeData, recordIdColumn = "entity.SampleID", name="test", imputeZero=T)

  # Add some missing values and ensure one sample gets dropped
  fakeCollection@data[1:50, "feat1"] <- NA
  fakeCollection@data[25:100, "feat2"] <- 0
  fakeCollection@data[(nSamples-50):nSamples, "feat3"] <- NA
  fakeCollection@data[26, "feat3"] <- NA

  output <- veupathUtils::pca(fakeCollection, nPCs = 2, ntop=5, verbose=T)
  expect_s4_class(output, "ComputeResult")
  outputData <- output@data
  expect_s3_class(outputData, "data.table")
  expect_equal(nrow(outputData), nSamples-1)
  expect_equal(ncol(outputData), 3)
  expect_equal(names(outputData), c("entity.SampleID", "PC1", "PC2"))
  computedVariableMetadata <- output@computedVariableMetadata
  expect_s4_class(computedVariableMetadata, "VariableMetadataList")
  expect_equal(length(computedVariableMetadata), 2)
  expect_s4_class(computedVariableMetadata[[1]], "VariableMetadata")
  expect_s4_class(computedVariableMetadata[[2]], "VariableMetadata")
  expect_equal(computedVariableMetadata[[1]]@variableSpec@variableId, "PC1")
  expect_equal(computedVariableMetadata[[2]]@variableSpec@variableId, "PC2")


  # Test with not enough features
  expect_error(veupathUtils::pca(fakeCollection, nPCs = 2, ntop=1, verbose=T), "ntop must be at least 2.")

})
