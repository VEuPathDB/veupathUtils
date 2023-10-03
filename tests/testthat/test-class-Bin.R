test_that("Bin validation works", {
  # start and end w different types
  expect_error(Bin(binStart=1, binEnd=as.Date('2020-03-14'), binLabel='oh no!'))

  # start but no end and vice versa
  expect_error(Bin(binStart=1, binLabel='doh!'))
  expect_error(Bin(binEnd=1, binLabel='done it again!'))

  # no bin label
  expect_error(Bin(binStart=1, binEnd=2))

  # start and end not coercible to numeric
  expect_error(Bin(binStart='a', binEnd='b', binLabel='ermahgerd!'))

  # too many starts, ends, labels or values
  expect_error(Bin(binStart=c(1,2), binEnd=c(2,3), binLabel=c('1-2','2-3')))
  expect_error(Bin(binLabel='im a good label!', value=c(1,2)))

  # start after end
  expect_error(Bin(binStart=2, binEnd=1, binLabel='uh oh!'))
})

test_that("BinList validation works", {
  bin1 <- Bin(binStart=1, binEnd=2, binLabel='1-2')
  bin2 <- Bin(binStart=1, binEnd=3, binLabel='1-3')
  bin3 <- Bin(binStart=2, binEnd=3, binLabel='2-3')
  bin4 <- Bin(binStart=2, binEnd=4, binLabel='2-4')
  bin5 <- Bin(binStart=2, binEnd=3, binLabel='2-3', value=1)
  bin6 <- Bin(binLabel='A')
  bin7 <- Bin(binLabel='B')
  # duplicate start or ends
  expect_error(BinList(S4Vectors::SimpleList(c(bin1, bin2))))
  expect_error(BinList(S4Vectors::SimpleList(c(bin2, bin3))))

  # overlapping ranges
  expect_error(BinList(S4Vectors::SimpleList(c(bin2, bin4))))

  # try to add an invalid Bin
  expect_error(BinList(S4Vectors::SimpleList(Bin(binStart=4, binEnd=2, binLabel='this shouldnt work'))))

  # not all have values when some do
  expect_error(BinList(S4Vectors::SimpleList(c(bin1, bin5))))

  # not all have start and ends when some do
  expect_error(BinList(S4Vectors::SimpleList(c(bin1, bin6))))

  # none have start or ends or value and thats ok
  expect_equal(length(BinList(S4Vectors::SimpleList(c(bin6, bin7)))), 2)
})

test_that("BinList can handle a list of one", {
  
  bin1 <- Bin(binStart=1, binEnd=2, binLabel='1-2')
  lonelyBinList <- BinList(S4Vectors::SimpleList(c(bin1)))
  expect_equal(length(lonelyBinList), 1)

})