# Test PCA

test_that("The pca function works", {
  
  testData <- testCountDataCollection
  
  # Run pca
  output <- pca(testData, nPCs = 2)
  expect_is(output, "data.table")
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
  expect_is(output, "data.table")
  expect_equal(nrow(output), nSamples)
  expect_equal(ncol(output), 6)
  expect_equal(names(output), c("entity.SampleID", "PC1", "PC2", "PC3", "PC4", "PC5"))
})
