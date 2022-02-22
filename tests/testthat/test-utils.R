## Tests for generic utils functions

test_that("getAggStr returns sane results", {
  expect_equal(getAggStr(c('a','b'),c(1,2,3)), "a + b ~ 1 + 2 + 3")
  expect_equal(getAggStr(c('a','b')), "a + b")
  expect_equal(getAggStr(NULL, c('a','b')), ". ~ a + b")
  expect_equal(getAggStr(), ".")
  expect_equal(getAggStr('a','b'), "a ~ b")
})

test_that("toColNameOrNull returns column names or NULL", {
  varDetailsList <- list('variableId' = 'varId', 'entityId' = 'entity')
  expect_equal(toColNameOrNull(varDetailsList), 'entity.varId')

  varDetailsList <- list('variableId' = 'varId')
  expect_equal(toColNameOrNull(varDetailsList), 'varId')

  varDetailsList <- list('entityId' = 'entity')
  expect_equal(toColNameOrNull(varDetailsList), NULL)

  # any other odd conditions to test here? empty char strings? NAs?
  expect_equal(toColNameOrNull(NULL), NULL)
})

# type conversion here behaves as expected once you think about it
# but we may want to revisit it, bc its not necessarily intuitive
# may serve as a source of bugs for the uncautious
# worth expanding to other types than character and logical ?
test_that("matchArg matches args, including booleans", {
  expect_equal(is.error(try(matchArg('a', c('a','b','c')))), FALSE)
  expect_equal(matchArg('a', c('a','b','c')), 'a')
  expect_equal(is.error(try(matchArg('a', c('A','b','c')))), TRUE)
  expect_equal(is.error(try(matchArg(1, c('A','b','c')))), TRUE)
  expect_equal(is.error(try(matchArg(T, c(T,F,'c')))), TRUE)
  expect_equal(is.error(try(matchArg(T, c(T,F)))), FALSE)
  expect_equal(matchArg(T, c(T,F)), T)
  expect_equal(is.error(try(matchArg(1, c(1,'b','c')))), TRUE)
  expect_equal(is.error(try(matchArg('1', c(1,'b','c')))), FALSE)
  expect_equal(matchArg('1', c(1,'b','c')), '1')

  expect_equal(matchArg(NULL, c(TRUE, FALSE)), TRUE)
  expect_equal(matchArg(c(TRUE, FALSE), c(TRUE, FALSE)), TRUE)
  expect_equal(matchArg(NULL, c('a', 'b')), 'a')
  expect_equal(matchArg(c('a', 'b'), c('a', 'b')), 'a')
})