test_that("whichValuesInBin works", {

  bin1 <- Bin(binStart=1, binEnd=2, binLabel='1-2')
  values <- c(0, 1, 2, 3)

  inBin <- whichValuesInBin(values, bin1)
  expect_equal(inBin, c(FALSE, TRUE, FALSE, FALSE))

  bin2 <- Bin(binStart=1, binEnd=2.0001, binLabel='1-2.0001')
  inBin <- whichValuesInBin(values, bin2)
  expect_equal(inBin, c(FALSE, TRUE, TRUE, FALSE))

  bin3 <- Bin(binStart=as.Date('2020-03-14'), binEnd=as.Date('2021-03-14'), binLabel='pieYear')
  dateValues <- as.Date(c('2020-02-14', '2020-03-14', '2020-08-14','2021-03-14','2021-05-14'))

  inBin <- whichValuesInBin(dateValues, bin3)
  expect_equal(inBin, c(FALSE, TRUE, TRUE, FALSE, FALSE))

})


test_that("whichValuesInBinList works", {

  bin1 <- Bin(binStart=1, binEnd=2, binLabel='1-2')
  bin2 <- Bin(binStart=3, binEnd=4, binLabel='3-4')
  binList <- BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  values <- c(0, 1, 2, 3, 4, 5)

  inBin <- whichValuesInBinList(values, binList)
  expect_equal(inBin, c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))

  # With Dates
  bin1 <- Bin(binStart=as.Date('1989-01-01'), binEnd=as.Date('1990-01-01'), binLabel='1989')
  bin2 <- Bin(binStart=as.Date('1991-01-01'), binEnd=as.Date('1992-01-01'), binLabel='1991')
  binList <- BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  values <- as.Date(c('1960-01-01','1989-01-01','1990-03-01','1991-01-01','1992-01-01','1999-01-01'))

  inBin <- whichValuesInBinList(values, binList)
  expect_equal(inBin, c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))

})

test_that("converting string bins to numeric bins is successful", {


  # Test for regular bins
  stringBin1 <- Bin(binStart='1', binEnd='2', binLabel='1-2', value = 2)
  
  numericBin1 <- as.numeric(stringBin1)
  expect_equal(class(numericBin1@binStart), 'numeric')
  expect_equal(class(numericBin1@binEnd), 'numeric')
  expect_equal(numericBin1@binLabel, stringBin1@binLabel)
  expect_equal(numericBin1@value, stringBin1@value)



  stringBin2 <- Bin(binStart='2.00001', binEnd='2e10', binLabel='2-2e10')
  
  numericBin2 <- as.numeric(stringBin2)
  expect_equal(class(numericBin2@binStart), 'numeric')
  expect_equal(class(numericBin2@binEnd), 'numeric')
  expect_equal(numericBin2@binLabel, stringBin2@binLabel)
  expect_equal(numericBin2@value, stringBin2@value)


  # Test for BinLists
  stringBin3 <- Bin(binStart='2e11', binEnd='2e12', binLabel='2e11-2e12')
  stringBinList1 <- BinList(S4Vectors::SimpleList(c(stringBin2, stringBin3)))

  numericBinList1 <- as.numeric(stringBinList1)
  expect_equal(unlist(lapply(numericBinList1, function(x) {class(x@binStart)})), c('numeric','numeric'))
  expect_equal(unlist(lapply(numericBinList1, function(x) {class(x@binEnd)})), c('numeric','numeric'))


})

