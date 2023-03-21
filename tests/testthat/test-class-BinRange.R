test_that("BinRange validation works", {
  # start and end w different types
  expect_error(BinRange(binStart=1, binEnd=as.Date('2020-03-14'), binLabel='oh no!'))

  # start but no end and vice versa
  expect_error(BinRange(binStart=1, binLabel='doh!'))
  expect_error(BinRange(binEnd=1, binLabel='done it again!'))

  # no bin label
  expect_error(BinRange(binStart=1, binEnd=2))

  # start and end not coercible to numeric
  expect_error(BinRange(binStart='a', binEnd='b', binLabel='ermahgerd!'))

  # too many starts, ends, labels or values
  expect_error(BinRange(binStart=c(1,2), binEnd=c(2,3), binLabel=c('1-2','2-3')))
  expect_error(BinRange(binLabel='im a good label!', value=c(1,2)))

  # start after end
  expect_error(BinRange(binStart=2, binEnd=1, binLabel='uh oh!'))
})

test_that("BinRangeList validation works", {
  bin1 <- BinRange(binStart=1, binEnd=2, binLabel='1-2')
  bin2 <- BinRange(binStart=1, binEnd=3, binLabel='1-3')
  bin3 <- BinRange(binStart=2, binEnd=3, binLabel='2-3')
  bin4 <- BinRange(binStart=2, binEnd=4, binLabel='2-4')
  bin5 <- BinRange(binStart=2, binEnd=3, binLabel='2-3', value=1)
  bin6 <- BinRange(binLabel='A')
  bin7 <- BinRange(binLabel='B')
  # duplicate start or ends
  expect_error(BinRangeList(S4Vectors::SimpleList(c(bin1, bin2))))
  expect_error(BinRangeList(S4Vectors::SimpleList(c(bin2, bin3))))

  # overlapping ranges
  expect_error(BinRangeList(S4Vectors::SimpleList(c(bin2, bin4))))

  # try to add an invalid BinRange
  expect_error(BinRangeList(S4Vectors::SimpleList(BinRange(binStart=4, binEnd=2, binLabel='this shouldnt work'))))

  # not all have values when some do
  expect_error(BinRangeList(S4Vectors::SimpleList(c(bin1, bin5))))

  # not all have start and ends when some do
  expect_error(BinRangeList(S4Vectors::SimpleList(c(bin1, bin6))))

  # none have start or ends or value and thats ok
  expect_equal(length(BinRangeList(S4Vectors::SimpleList(c(bin6, bin7)))), 2)
})