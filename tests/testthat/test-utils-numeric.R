context("numericUtils")

test_that("nonZeroRound only returns 0 if it receives one", {
  expect_equal(nonZeroRound(0),0)
  expect_equal(nonZeroRound(123456789.987654321, 4) == 0, FALSE)
  expect_equal(nonZeroRound(0.987654321, 4) == 0, FALSE)
  expect_equal(nonZeroRound(0.00000019, 4) == 0, FALSE)
  expect_equal(nonZeroRound(0.00000019, 4), 0.0000002)
})

test_that("setNaToValue replaces intended NAs", {
  
  df <- iris

  # Add NAs to all columns
  nMissing <- 10
  df <- lapply(df, function(x) {x[sample(1:length(x), size=nMissing)] <- NA; return(x)})

  # With specified columns
  setNaToValue(df, value=1000, cols=c("Sepal.Length", "Sepal.Width"))
  expect_equal(sum(is.na(df)), 3*nMissing)
  expect_equal(sum(df[, c('Sepal.Length', 'Sepal.Width')] == 1000), 2*nMissing)

  # With defualt. Should change only numeric columns
  setNaToValue(df, value=500)
  expect_equal(sum(is.na(df)), nMissing)
  expect_equal(sum(df[, c('Petal.Length', 'Petal.Width')] == 500), 2*nMissing)

})