test_that("breaks returns appropriate results", {
  
  df <- rnorm(500, 20, 5)
  
  dt <- breaks(df, 'quantile', nbins=20)
  expect_equal(length(dt), 21)
  
  dt <- breaks(df, 'quantile', binwidth=0.5)
  expect_equal(length(dt), 3)
  expect_equal(names(dt), c('0%','50%','100%'))
  
  dt <- breaks(c(1,2), 'equalInterval', binwidth=0.5)
  expect_equal(dt, c(1, 1.5, 2))

  dt <- breaks(df, 'sd')
  expect_equal(all.equal(dt, c(min(min(df),median(df) - 3*sd(df)), 
                               median(df) - 2*sd(df), 
                               median(df) - sd(df), 
                               median(df), 
                               median(df) + sd(df), 
                               median(df) + 2*sd(df), 
                               max(max(df),median(df) + 3*sd(df))), tolerance=.5), TRUE)
})

test_that("cut_width splits the first inclusive bin for integers where bin width is 1", {
  ints <- c(0,1,2,2,3,3,3,4,4,4,4)
  expectedBins <- c("[-1,0]","(0,1]","(1,2]","(1,2]","(2,3]","(2,3]","(2,3]","(3,4]","(3,4]","(3,4]","(3,4]")

  expect_equal(as.character(cut_width(ints, 1, .5)), expectedBins)
  expect_equal(as.character(cut_width(ints,1,boundary=min(ints))), expectedBins) 
})