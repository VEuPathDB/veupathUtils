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