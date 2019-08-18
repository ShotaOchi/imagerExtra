test_that("local adaptive thresholding",
{
  k_c <- 0.1
  k_bad1 <- NA
  k_bad2 <- 11
  
  windowsize_bad1 <- NA
  windowsize_bad2 <- 2
  windowsize_bad3 <- 12
  
  range_bad3 <- c(NA, 255)

  expect_error(ThresholdAdaptive(gim_bad, k_c))

  expect_error(ThresholdAdaptive(gim, k_bad1))
  expect_error(ThresholdAdaptive(gim, k_bad2))
  
  expect_error(ThresholdAdaptive(gim, k_c, range = range_bad1))
  expect_error(ThresholdAdaptive(gim, k_c, range = range_bad4))

  expect_error(ThresholdAdaptive(gim, k_c, windowsize_bad1))
  expect_error(ThresholdAdaptive(gim, k_c, windowsize_bad2))
  expect_warning(ThresholdAdaptive(gim, k_c, windowsize_bad3))
  
  expect_class(ThresholdAdaptive(gim, k_c), class_pixset)
})
