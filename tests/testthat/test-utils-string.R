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

test_that("strSplit splits only on the first delimiter", {
  # strings with more delimiters than expected must not be mis-shaped into a matrix
  expect_equal(strSplit("once upon a time", " "), "once")
  expect_equal(strSplit("once upon a time", " ", index = 2), "upon a time")
  expect_equal(strSplit(c("once upon a time", "a b"), " "), c("once", "a"))
  expect_equal(strSplit(c("once upon a time", "a b"), " ", index = 2), c("upon a time", "b"))

  # e.g. an entity.variable id where the variable id itself contains periods
  expect_equal(strSplit("entity.PF3D7_0100100.1", "."), "entity")
  expect_equal(strSplit("entity.PF3D7_0100100.1", ".", index = 2), "PF3D7_0100100.1")
  expect_equal(strSplit(c("entity.gene.1", "entity.gene.2", "entity.gene"), ".", index = 2),
               c("gene.1", "gene.2", "gene"))
  expect_equal(strSplit("entity.a.b.c", ".", 4, 2), "a.b.c")

  # regex patterns
  expect_equal(strSplit("a1b2c", "[0-9]", 2, 2, fixed = FALSE), "b2c")
})

