context("numericUtils")

test_that("nonZeroRound only returns 0 if it receives one", {
  expect_equal(nonZeroRound(0),0)
  expect_equal(nonZeroRound(123456789.987654321, 4) == 0, FALSE)
  expect_equal(nonZeroRound(0.987654321, 4) == 0, FALSE)
  expect_equal(nonZeroRound(0.00000019, 4) == 0, FALSE)
  expect_equal(nonZeroRound(0.00000019, 4), 0.0000002)
})