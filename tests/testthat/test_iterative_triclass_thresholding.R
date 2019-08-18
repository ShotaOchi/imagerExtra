test_that("Iterative Triclass Thresholding",
{
  bad <- NA
  
  expect_error(ThresholdTriclass(gim_bad))
  expect_error(ThresholdTriclass(gim_uniform))
  
  expect_error(ThresholdTriclass(gim, stopval = bad))
  
  expect_error(ThresholdTriclass(gim, repeatnum = bad))
  expect_error(ThresholdTriclass(gim, repeatnum = 0.1))
  
  expect_error(ThresholdTriclass(gim, returnvalue = bad))
  
  expect_error(ThresholdTriclass(gim, intervalnumber = bad))
  
  expect_class(ThresholdTriclass(gim), class_pixset)
  expect_class(ThresholdTriclass(gim, returnvalue = TRUE), "numeric")
})