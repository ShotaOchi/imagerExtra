test_that("fuzzy thresholding",
{
  bad <- NA
  
  expect_error(ThresholdFuzzy(gim_bad))
  expect_error(ThresholdFuzzy(gim_uniform))
  
  expect_error(ThresholdFuzzy(gim, returnvalue = bad))
  
  expect_error(ThresholdFuzzy(gim, n = bad))
  expect_error(ThresholdFuzzy(gim, n = 0.1))
  
  expect_error(ThresholdFuzzy(gim, maxiter = bad))
  expect_error(ThresholdFuzzy(gim, maxiter = 1))
  
  expect_error(ThresholdFuzzy(gim, intervalnumber = bad))
  expect_error(ThresholdFuzzy(gim, intervalnumber = 1))
  
  expect_error(ThresholdFuzzy(gim, c1 = bad))
  expect_error(ThresholdFuzzy(gim, c2 = bad))
  
  expect_error(ThresholdFuzzy(gim, mutrate = bad))
  
  expect_error(ThresholdFuzzy(gim, vmaxcoef = bad))
  
  expect_error(ThresholdFuzzy(gim, omegamax = bad))
  expect_error(ThresholdFuzzy(gim, omegamax = 1))
  
  expect_error(ThresholdFuzzy(gim, omegamin = bad))
  expect_error(ThresholdFuzzy(gim, omegamin = 1))
  
  expect_error(ThresholdFuzzy(gim, omegamax = 0.2, omegamin = 0.5))
  
  expect_class(ThresholdFuzzy(gim), class_pixset)
  expect_class(ThresholdFuzzy(gim, returnvalue = TRUE), "numeric")
})
