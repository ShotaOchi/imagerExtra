test_that("multilevel thresholding",
{
  bad1 <- NA
  
  k_c <- 2
  
  vec_good <- c(0.1,0.5)
  vec_bad <- c(1, NA)
  vec_badorder <- c(0.5, 0.1)
  
  thr_bad <- "error"
  
  expect_error(ThresholdML(gim_bad, k_c))
  expect_error(ThresholdML(gim_uniform, k_c))
  
  expect_error(ThresholdML(gim, bad1))
  expect_error(ThresholdML(gim, 0.1))
  
  expect_error(ThresholdML(gim, k_c, returnvalue = bad1))
  
  expect_error(ThresholdML(gim, k_c, thr = bad1))
  expect_error(ThresholdML(gim, thr = vec_bad))
  expect_error(ThresholdML(gim, thr = thr_bad))
  expect_warning(ThresholdML(gim, thr = vec_badorder))
  
  expect_error(ThresholdML(gim, k_c, thr = "manual", sn = bad1))
  expect_error(ThresholdML(gim, k_c, thr = "manual", sn = 1))
  
  expect_error(ThresholdML(gim, k_c, thr = "manual", mcn = bad1))
  expect_error(ThresholdML(gim, k_c, thr = "manual", mcn = 0.1))
  
  expect_error(ThresholdML(gim, k_c, thr = "manual", limit = bad1))
  expect_error(ThresholdML(gim, k_c, thr = "manual", limit = 0.1))
  
  expect_error(ThresholdML(gim, k_c, thr = "manual", intervalnumber = bad1))
  expect_error(ThresholdML(gim, k_c, thr = "manual", intervalnumber = 1))
  
  expect_class(ThresholdML(gim, k_c, returnvalue = TRUE), "numeric")
  expect_class(ThresholdML(gim, k_c), class_imager)
  expect_class(ThresholdML(gim, k_c, thr = "precise", returnvalue = TRUE), "numeric")
  expect_class(ThresholdML(gim, k_c, thr = "precise"), class_imager)
  expect_class(ThresholdML(gim, k_c, thr = "manual", returnvalue = TRUE), "numeric")
  expect_class(ThresholdML(gim, k_c, thr = "manual"), class_imager)
  expect_class(ThresholdML(gim, thr = vec_good, returnvalue = TRUE), "numeric")
  expect_class(ThresholdML(gim, thr = vec_good), class_imager)
})
