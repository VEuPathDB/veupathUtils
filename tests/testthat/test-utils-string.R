## Tests for string utils functions

test_that("toStringOrNull has sane behavior", {
  expect_equal(toStringOrNull(NULL), NULL)
  expect_equal(toStringOrNull(NA), NULL)
  expect_equal(toStringOrNull(), NULL)
  expect_equal(toStringOrNull(""), NULL)
  expect_equal(toStringOrNull(character()), NULL)
  expect_equal(toStringOrNull("a"), "a")
  expect_equal(toStringOrNull(1), "1")
  expect_equal(toStringOrNull(as.Date('2021-12-25')), "2021-12-25")
  expect_equal(toStringOrNull(T), "TRUE") 

  
  expect_equal(toStringOrNull(c(NA, 1, 'b')), c('1', 'b'))
  expect_equal(toStringOrNull(c(NA, 1, 'b'), na.rm=FALSE), c('NA', '1', 'b'))
  expect_equal(toStringOrNull(c(NA, "", character())), NULL)
  expect_equal(toStringOrNull(c(NA, "", character()), na.rm=FALSE), "NA")

  #this behavior is probably unintuitive.
  expect_equal(toStringOrNull(list(NA, 1, 'b')), c('1', 'b'))
  expect_equal(toStringOrNull(list(NA, "", character())), NULL)
})

test_that("trim trims", {
  expect_equal(trim("a"), "a")
  expect_equal(trim("  a  "), "a")
  expect_equal(trim("  a"), "a")
  expect_equal(trim("  a\t"), "a")
  expect_equal(trim("\na  "), "a")
  expect_equal(trim("\t\tabc\n\n\t"), "abc")
  expect_equal(trim(c("a", "  b", "\tc")), c("a","b","c"))
})

test_that("strSplit splits strings", {
  expect_equal(strSplit(c('hello world', 'foo bar'), "\\s", 2, 1, F), c('hello', 'foo'))
  expect_equal(strSplit(c('hello-world', 'foo-bar'), "-"), c('hello', 'foo'))
  expect_equal(strSplit(c('hello world', 'foo bar'), "\\s", 2, 2, F), c('world', 'bar'))
  expect_equal(strSplit(c('hello-world', 'foo-bar'), "-", 2, 2), c('world', 'bar'))

  #expect_equal(strSplit(c('hello-world', 'foo-bar', 'a', '-', 3), "-"), c('hello', 'foo', 'a', '', '3'))
})