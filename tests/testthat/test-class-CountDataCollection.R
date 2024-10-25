test_that('CountDataCollection validation works', {

  # Most tests are handled by the parent class.

  df <- testCountData  # A count dataset derived from mbio

  testing <- veupathUtils::CountDataCollection(
              data = df,
              recordIdColumn = c('entity.SampleID'),
              name='testCountData'
  )

  expect_true(inherits(testing, 'CollectionWithMetadata'))
  expect_true(inherits(testing, 'CountDataCollection'))
  expect_equal(sort(slotNames(testing)), c('ancestorIdColumns', 'data', 'imputeZero', 'name', 'recordIdColumn', 'removeEmptyRecords', 'sampleMetadata'))
  expect_equal(nrow(testing@data), 288)
  expect_equal(ncol(testing@data), 909)
  expect_equal(testing@recordIdColumn, 'entity.SampleID')
  expect_equal(testing@ancestorIdColumns, character(0))
  expect_true(testing@imputeZero)

  # Expect error when input data is not integers (not rounded)
  df_error <- data.table::copy(df)
  df_error$entity.notAnInteger <- df_error$entity.A2 + 0.01
  expect_error(veupathUtils::CountDataCollection(
              data = df_error,
              recordIdColumn = c('entity.SampleID'),
              name='testCountData'
              ))


  
})