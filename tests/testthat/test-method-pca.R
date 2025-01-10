# Test PCA

test_that("The pca function works", {
  
  testData <- testCountDataCollection
  
  # Run pca
  output <- pca(testData, nPCs = 2)
  expect_s3_class(output, "data.table")
  expect_equal(nrow(output), nrow(getCollectionData(testData)))
  expect_equal(ncol(output), 3)
  expect_equal(names(output), c("entity.SampleID", "PC1", "PC2"))

  
  # Generate some fake data
  nSamples <- 500
  fakeData <- data.table(
    entity.SampleID = paste0("Sample", 1:nSamples),
    feat1 = rnorm(nSamples),
    feat2 = rnorm(nSamples),
    feat3 = rnorm(nSamples)
  )

  fakeCollection <- new("Collection", data = fakeData, recordIdColumn = "entity.SampleID", name="test")

  output <- pca(fakeCollection, nPCs = 2, ntop=20)
  expect_s3_class(output, "data.table")
  expect_equal(nrow(output), nSamples)
  expect_equal(ncol(output), 3)
  expect_equal(names(output), c("entity.SampleID", "PC1", "PC2"))
})

test_that("The pca function can handle messy data", {

  # Generate some fake data
  nSamples <- 500
  fakeData <- data.table(
    entity.SampleID = paste0("Sample", 1:nSamples),
    feat1 = rnorm(nSamples),
    feat2 = rnorm(nSamples),
    feat3 = rnorm(nSamples)
  )

  fakeCollection <- new("Collection", data = fakeData, recordIdColumn = "entity.SampleID", name="test", imputeZero=T)

  # Add some missing values and ensure one sample gets dropped
  fakeCollection@data[1:50, "feat1"] <- NA
  fakeCollection@data[25:100, "feat2"] <- 0
  fakeCollection@data[(nSamples-50):nSamples, "feat3"] <- NA
  fakeCollection@data[26, "feat3"] <- NA

  output <- pca(fakeCollection, nPCs = 2, ntop=5, verbose=T)
  expect_s3_class(output, "data.table")
  expect_equal(nrow(output), nSamples-1)
  expect_equal(ncol(output), 3)
  expect_equal(names(output), c("entity.SampleID", "PC1", "PC2"))


  # Test with not enough features
  expect_error(pca(fakeCollection, nPCs = 2, ntop=1, verbose=T), "ntop must be at least 2.")

})
