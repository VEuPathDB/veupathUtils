# Tests for ArrayDataCollection class

test_that('ArrayDataCollection validation works', {

  df <- testArrayData

  testing <- veupathUtils::ArrayDataCollection(
    data = df,
    recordIdColumn = c('entity.SampleID'),
    name = 'testArrayData'
  )

  expect_true(inherits(testing, 'CollectionWithMetadata'))
  expect_true(inherits(testing, 'ArrayDataCollection'))
  expect_equal(sort(slotNames(testing)), c('ancestorIdColumns', 'data', 'imputeZero', 'name', 'recordIdColumn', 'removeEmptyRecords', 'sampleMetadata'))
  expect_equal(nrow(testing@data), 50)
  expect_equal(ncol(testing@data), 101)  # 100 antibodies + 1 ID column
  expect_equal(testing@recordIdColumn, 'entity.SampleID')
  expect_equal(testing@ancestorIdColumns, character(0))
  expect_true(testing@imputeZero)
})

test_that('ArrayDataCollection accepts continuous data (not just integers)', {

  df <- data.table::copy(testArrayData)
  df$entity.antibody_001 <- df$entity.antibody_001 + 0.5

  expect_no_error(veupathUtils::ArrayDataCollection(
    data = df,
    recordIdColumn = c('entity.SampleID'),
    name = 'testArrayData'
  ))
})

test_that('ArrayDataCollection accepts negative values', {

  df <- data.table::copy(testArrayData)
  df$entity.antibody_001 <- -abs(df$entity.antibody_001)

  expect_no_error(veupathUtils::ArrayDataCollection(
    data = df,
    recordIdColumn = c('entity.SampleID'),
    name = 'testArrayData'
  ))
})

test_that('ArrayDataCollection rejects non-numeric data', {

  df <- data.table::copy(testArrayData)
  df$entity.notNumeric <- "not_numeric"

  expect_error(veupathUtils::ArrayDataCollection(
    data = df,
    recordIdColumn = c('entity.SampleID'),
    name = 'testArrayData'
  ))
})

test_that('ArrayDataCollection works with sampleMetadata', {

  # This is how testArrayDataCollection was created
  expect_true(inherits(testArrayDataCollection, 'ArrayDataCollection'))
  expect_equal(nrow(testArrayDataCollection@data), 50)
  expect_equal(ncol(testArrayDataCollection@data), 101)
  expect_equal(nrow(testArrayDataCollection@sampleMetadata@data), 50)
  expect_true('entity.treatment' %in% names(testArrayDataCollection@sampleMetadata@data))
})

test_that('ArrayDataCollection is different from CountDataCollection', {

  # The continuous array data should fail CountDataCollection validation
  # because it has non-integer values
  expect_error(veupathUtils::CountDataCollection(
    data = testArrayData,
    recordIdColumn = c('entity.SampleID'),
    name = 'testArrayData'
  ), "integer")

  # While it should work fine for ArrayDataCollection
  expect_no_error(veupathUtils::ArrayDataCollection(
    data = testArrayData,
    recordIdColumn = c('entity.SampleID'),
    name = 'testArrayData'
  ))

  # Similarly, data with negative values should fail CountDataCollection
  negativeData <- data.table::copy(testArrayData)
  negativeData[[2]] <- -abs(negativeData[[2]])
  expect_error(veupathUtils::CountDataCollection(
    data = negativeData,
    recordIdColumn = c('entity.SampleID'),
    name = 'testArrayData'
  ), "negative")

  # But works for ArrayDataCollection
  expect_no_error(veupathUtils::ArrayDataCollection(
    data = negativeData,
    recordIdColumn = c('entity.SampleID'),
    name = 'testArrayData'
  ))
})
