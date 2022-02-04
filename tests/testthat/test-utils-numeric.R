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

  # With defualt. Should change only numeric columns
  dt <- as.data.table(df)
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

  # With defualt. Should change only numeric columns
  dt <- as.data.table(df)
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
  expect_true(any(!is.na(mat)))
  expect_equal(sum(mat == 0), nMissing)

  # Do nothing to strings
  vec <- c('1','2','3',NA)
  vec <- naToZero(vec)
  expect_equal(class(vec), 'character')
  expect_equal(vec, c('1','2','3',NA))

})