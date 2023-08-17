test_that("whichValuesInBin works", {

  bin1 <- Bin(binStart=1, binEnd=2, binLabel='1-2')
  values <- c(0, 1, 2, 3)

  inBin <- whichValuesInBin(values, bin1)
  expect_equal(inBin, c(FALSE, TRUE, FALSE, FALSE))

  bin2 <- Bin(binStart=1, binEnd=2.0001, binLabel='1-2.0001')
  inBin <- whichValuesInBin(values, bin2)
  expect_equal(inBin, c(FALSE, TRUE, TRUE, FALSE))

})


test_that("whichValuesInBinList works", {

  bin1 <- Bin(binStart=1, binEnd=2, binLabel='1-2')
  bin2 <- Bin(binStart=3, binEnd=4, binLabel='3-4')
  binList <- BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  values <- c(0, 1, 2, 3, 4, 5)

  inBin <- whichValuesInBinList(values, binList)
  expect_equal(inBin, c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))

})
