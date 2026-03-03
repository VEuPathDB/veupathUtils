# Test PCA

test_that("deseqNormalize applies median-of-ratios normalization", {
  # Simple 3-gene x 3-sample count matrix
  countMatrix <- matrix(
    c(100, 200, 50,
      110, 180, 55,
       90, 220, 45),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("g1", "g2", "g3"), c("s1", "s2", "s3"))
  )

  normalized <- veupathUtils::deseqNormalize(countMatrix)

  # Output should have same dimensions and dimnames
  expect_equal(dim(normalized), dim(countMatrix))
  expect_equal(dimnames(normalized), dimnames(countMatrix))

  # Size factors should make column medians of ratios ~1
  # i.e. the normalized values should differ from the raw values
  expect_false(identical(normalized, countMatrix))

  # Verify against DESeq2 directly
  sizeFactors <- DESeq2::estimateSizeFactorsForMatrix(countMatrix)
  expected <- sweep(countMatrix, 2, sizeFactors, FUN = "/")
  expect_equal(normalized, expected)
})

test_that("The pca function produces the expected output", {
  
  testData <- testCountDataCollection
  
  # Run pca
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
  expect_equal(computedVariableMetadata[[1]]@displayName, "PC 1 (15.53% variance)")
  expect_equal(computedVariableMetadata[[2]]@displayName, "PC 2 (14.13% variance)")

  
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

  fakeCollection <- new("Collection", 
    data = fakeData,
    recordIdColumn = "entity.SampleID",
    name="test",
    imputeZero=TRUE
  )

  # Add some missing values and ensure one sample gets dropped
  fakeCollection@data[1:50, "entity.feat1"] <- NA
  fakeCollection@data[25:100, "entity.feat2"] <- 0
  fakeCollection@data[(nSamples-50):nSamples, "entity.feat3"] <- NA
  fakeCollection@data[26, "entity.feat3"] <- NA

  output <- pca(fakeCollection, nPCs = 3, ntop=5, verbose=T)
  expect_s4_class(output, "ComputeResult")
  outputData <- output@data
  expect_s3_class(outputData, "data.table")
  expect_equal(nrow(outputData), nSamples-1)
  expect_equal(ncol(outputData), 4)
  expect_equal(names(outputData), c("entity.SampleID", "PC1", "PC2", "PC3"))
  computedVariableMetadata <- output@computedVariableMetadata
  expect_s4_class(computedVariableMetadata, "VariableMetadataList")
  expect_equal(length(computedVariableMetadata), 3)
  expect_s4_class(computedVariableMetadata[[1]], "VariableMetadata")
  expect_s4_class(computedVariableMetadata[[2]], "VariableMetadata")
  expect_s4_class(computedVariableMetadata[[3]], "VariableMetadata")
  expect_equal(computedVariableMetadata[[1]]@variableSpec@variableId, "PC1")
  expect_equal(computedVariableMetadata[[1]]@variableSpec@entityId, "entity")
  expect_equal(computedVariableMetadata[[2]]@variableSpec@variableId, "PC2")
  expect_equal(computedVariableMetadata[[2]]@variableSpec@entityId, "entity")
  expect_equal(computedVariableMetadata[[3]]@variableSpec@variableId, "PC3")
  expect_equal(computedVariableMetadata[[3]]@variableSpec@entityId, "entity")


  # Test with not enough features
  expect_error(veupathUtils::pca(fakeCollection, nPCs = 2, ntop=1, verbose=T), "ntop must be at least 2.")

})

test_that("pca with normalize=TRUE applies DESeq2-style normalization", {

  testData <- testCountDataCollection

  # Run with and without normalization
  outputNorm <- veupathUtils::pca(testData, nPCs = 2, normalize = TRUE)
  outputRaw <- veupathUtils::pca(testData, nPCs = 2, normalize = FALSE)

  # Both should produce valid ComputeResult objects
  expect_s4_class(outputNorm, "ComputeResult")
  expect_s4_class(outputRaw, "ComputeResult")

  # Same structure
  expect_equal(nrow(outputNorm@data), nrow(outputRaw@data))
  expect_equal(ncol(outputNorm@data), ncol(outputRaw@data))
  expect_equal(names(outputNorm@data), names(outputRaw@data))

  # But different PC values (normalization should change the result)
  expect_false(all(outputNorm@data$PC1 == outputRaw@data$PC1))

  # Variance percentages should differ
  expect_false(outputNorm@computedVariableMetadata[[1]]@displayName ==
               outputRaw@computedVariableMetadata[[1]]@displayName)

  # Parameters should record normalize = TRUE
  expect_true(grepl("normalize = TRUE", outputNorm@parameters))
  expect_true(grepl("normalize = FALSE", outputRaw@parameters))
})
