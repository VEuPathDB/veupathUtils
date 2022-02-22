## Tests for numeric utils functions

test_that("nonZeroRound only returns 0 if it receives one", {
  expect_equal(nonZeroRound(0),0)
  expect_equal(nonZeroRound(123456789.987654321, 4) == 0, FALSE)
  expect_equal(nonZeroRound(0.987654321, 4) == 0, FALSE)
  expect_equal(nonZeroRound(0.00000019, 4) == 0, FALSE)
  expect_equal(nonZeroRound(0.00000019, 4), 0.0000002)
})

test_that("setNaToZero replaces intended NAs", {
  
  df <- iris

  # Add NAs to all columns
  nMissing <- 10
  df <- as.data.frame(lapply(df, function(x) {x[sample(1:length(x), size=nMissing)] <- NA; return(x)}))

  # With specified columns
  setNaToZero(df, cols=c("Sepal.Length", "Sepal.Width"))
  expect_equal(class(df), 'data.frame')
  expect_equal(sum(is.na(df)), 3*nMissing)
  expect_equal(sum(df[, c('Sepal.Length', 'Sepal.Width')] == 0), 2*nMissing)

  # Err if given a non-numeric column
  expect_error(naToZero(df, cols = c("Sepal.Length", "Sepal.Width", "Species")))

  # With defualt cols and data.table. Should change only numeric columns
  dt <- data.table::as.data.table(df)
  setNaToZero(dt)
  expect_equal(class(dt), c('data.table','data.frame'))
  expect_equal(sum(is.na(dt)), nMissing)
  expect_equal(sum(dt[, c('Petal.Length', 'Petal.Width')] == 0), 2*nMissing)

})

test_that("naToZero replaces intended NAs", {

  df <- iris

  # Add NAs to all columns
  nMissing <- 10
  df <- as.data.frame(lapply(df, function(x) {x[sample(1:length(x), size=nMissing)] <- NA; return(x)}))

  # With specified columns
  df <- naToZero(df, cols=c("Sepal.Length", "Sepal.Width"))
  expect_equal(class(df), 'data.frame')
  expect_equal(sum(is.na(df)), 3*nMissing)
  expect_equal(sum(df[, c('Sepal.Length', 'Sepal.Width')] == 0), 2*nMissing)

  # With defualt cols and data.table input. Should change only numeric columns
  dt <- data.table::as.data.table(df)
  dt <- naToZero(dt)
  expect_equal(class(dt), c('data.table','data.frame'))
  expect_equal(sum(is.na(dt)), nMissing)
  expect_equal(sum(dt[, c('Petal.Length', 'Petal.Width')] == 0), 2*nMissing)

  # Testing additional object types
  # Lists
  lst <- lapply(iris, function(x) {x[sample(1:length(x), size=nMissing)] <- NA; return(x)})
  lst <- naToZero(lst)
  expect_equal(class(lst), 'list')
  expect_equal(sum(unlist(lapply(lst, function(x) {sum(is.na(x))}))), nMissing)
  expect_equal(sum(unlist(lapply(lst[c('Petal.Length', 'Petal.Width')], function(x) {sum(x==0)}))), 2*nMissing)

  # Err if given a non-numeric column
  expect_error(naToZero(lst, cols = c("Sepal.Length", "Sepal.Width", "Species")))


  # Vector
  vec <- c(1,2,3,NA)
  vec <- naToZero(vec)
  expect_equal(class(vec), 'numeric')
  expect_true(all(!is.na(vec)))
  expect_equal(vec, c(1,2,3,0))

  # Matrix
  mat <- matrix(rnorm(36), nrow=6)
  mat[sample(1:36, size=nMissing, replace=F)] <- NA
  mat <- naToZero(mat)
  expect_equal(class(mat), c('matrix', 'array'))
  expect_true(any(!is.na(mat)))
  expect_equal(sum(mat == 0), nMissing)

  # Do nothing to strings
  vec <- c('1','2','3',NA)
  vec <- naToZero(vec)
  expect_equal(class(vec), 'character')
  expect_equal(vec, c('1','2','3',NA))

})


test_that("finding and validating numeric columns works", {

  df <- iris

  # With specified columns
  numericCols <- findNumericCols(df)
  expect_equal(numericCols, c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'))

  # In a data.table
  dt <- data.table::as.data.table(df)
  numericCols <- findNumericCols(dt)
  expect_equal(numericCols, c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'))
  
  validatedCols <- validateNumericCols(dt, c('Sepal.Length', 'Sepal.Width'))
  expect_equal(validatedCols, c('Sepal.Length', 'Sepal.Width'))

  validatedCols <- validateNumericCols(dt, c(1, 2, 3))
  expect_equal(validatedCols, c(1, 2, 3))

  # In a list
  lst <- as.list(iris)
  numericCols <- findNumericCols(lst)
  expect_equal(numericCols, c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'))

  validatedCols <- validateNumericCols(lst, c('Sepal.Length', 'Sepal.Width'))
  expect_equal(validatedCols, c('Sepal.Length', 'Sepal.Width'))

  validatedCols <- validateNumericCols(lst, c(2, 3, 4))
  expect_equal(validatedCols, c(2, 3, 4))

  # If no numeric cols
  dt_string <- dt[, lapply(.SD, as.character)]
  numericCols <- findNumericCols(dt_string)
  expect_equal(numericCols, NULL)

  lst_string <- lapply(iris, function(x) {x <- as.character(x); return(x)})
  numericCols <- findNumericCols(lst_string)
  expect_equal(numericCols, NULL)

  # validateNumericCols should err if given non-numeric column names
  expect_error(validateNumericCols(dt, c('Sepal.Length', 'Species')))
  expect_error(validateNumericCols(lst, c('Sepal.Length', 'Species')))

  # err if column names do not exist
  expect_error(validateNumericCols(dt, c('a', 'Species')))
  expect_error(validateNumericCols(lst, c('a', 'Species')))

  # err if indices too large
  expect_error(validateNumericCols(dt, c(1, 2, 100)))
  expect_error(validateNumericCols(lst, c(1, 2, 100)))

  # err if indices too small
  expect_error(validateNumericCols(dt, c(-1, 2, 100)))
  expect_error(validateNumericCols(lst, c(-1, 2, 100)))

  # return NULL for NULL input
  validatedCols <- validateNumericCols(dt, NULL)
  expect_equal(validatedCols, NULL)

  validatedCols <- validateNumericCols(lst, NULL)
  expect_equal(validatedCols, NULL)

  # remove NAs in cols arg
  validatedCols <- validateNumericCols(dt, c(1, NA, 4))
  expect_equal(validatedCols, c(1, 4))

  validatedCols <- validateNumericCols(lst, c(1, NA, 4))
  expect_equal(validatedCols, c(1, 4))

})