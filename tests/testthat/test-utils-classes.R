## Tests for class utils functions

test_that("is.POSIXct has sane results", {
  expect_equal(is.POSIXct('a'), FALSE)
  expect_equal(is.POSIXct(1), FALSE)
  expect_equal(is.POSIXct(FALSE), FALSE)
  expect_equal(is.POSIXct(as.POSIXct('2021-12-25')), TRUE)
  expect_equal(is.POSIXct(as.POSIXlt('2021-12-25')), FALSE)
  expect_equal(is.POSIXct(as.Date('2021-12-25')), FALSE)
})

test_that("is.error has sane results", {
  expect_equal(is.error(try(matchArg(F, c('a','b')))), TRUE)

  #warning only
  dataDF <- data.frame(iris$Sepal.Length, iris$Sepal.Width)
  expect_equal(is.error(try(chisq.test(table(dataDF)))), FALSE)
 
  expect_equal(is.error(try(TRUE)), FALSE)
})

test_that("data_frame returns data.frame", {
  expect_equal(data_frame('col'=c('a','b','c')),data.frame('col'=c('a','b','c')))
  expect_equal(class(data_frame('col'=c('a','b','c'))), 'data.frame')
  expect_equal(data_frame('col'=c('a','b','c'),
                          'col2'=c(1,2,3)),
               data.frame('col'=c('a','b','c'),
                          'col2'=c(1,2,3)))
  expect_equal(class(data_frame('col'=c('a','b','c'),
                          'col2'=c(1,2,3))), 'data.frame')
  #col names required for data_frame
  expect_equal(is.error(try(data_frame(c('a','b','c')))), TRUE)
})

test_that("setAttrFromList sets attributes from a list", {
  irisDT <- data.table::as.data.table(iris)
  attr <- list('class'='dummy', 'newAttr'='test')
  setAttrFromList(irisDT, attr)
  expect_equal(names(attributes(irisDT)), c('class', 'newAttr'))
  expect_equal(attributes(irisDT)$class, 'dummy')
  expect_equal(attributes(irisDT)$newAttr, 'test')

  irisDT <- data.table::as.data.table(iris)
  setAttrFromList(irisDT, attr, removeExtraAttrs = FALSE)
  expect_equal(names(attributes(irisDT)), c('names', 'row.names', 'class', '.internal.selfref', 'newAttr'))
  expect_equal(attributes(irisDT)$class, 'dummy')
  expect_equal(attributes(irisDT)$newAttr, 'test')
})