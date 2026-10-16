## Tests for generic utils functions

test_that("getAggStr returns sane results", {
  expect_equal(getAggStr(c('a','b'),c(1,2,3)), "a + b ~ 1 + 2 + 3")
  expect_equal(getAggStr(c('a','b')), "a + b")
  expect_equal(getAggStr(NULL, c('a','b')), ". ~ a + b")
  expect_equal(getAggStr(), ".")
  expect_equal(getAggStr('a','b'), "a ~ b")
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
test_that("stripEntityIdFromColumnHeader handles variable ids containing periods", {
  expect_equal(stripEntityIdFromColumnHeader(c("entity.id", "entity.var")), c("id", "var"))
  expect_equal(stripEntityIdFromColumnHeader(c("entity.id", "entity.PF3D7_01.1", "entity.PF3D7_02.1")),
               c("id", "PF3D7_01.1", "PF3D7_02.1"))
  # an odd number of pieces overall used to trigger a matrix error
  expect_equal(stripEntityIdFromColumnHeader(c("entity.a.b")), "a.b")
  # columns without an entity prefix are left alone
  expect_equal(stripEntityIdFromColumnHeader(c("plain", "entity.a.b")), c("plain", "a.b"))
})
