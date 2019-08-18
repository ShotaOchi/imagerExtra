test_that("piecewise_equalization", 
{
  N <- 100
  bad1 <- NA
  bad2 <- 0
  
  expect_error(EqualizePiecewise(gim_bad, N))
  expect_error(EqualizePiecewise(gim, bad1))
  expect_class(EqualizePiecewise(gim, bad2), class_imager)
  expect_error(EqualizePiecewise(gim, N, range = range_bad1))
  expect_error(EqualizePiecewise(gim, N, smax = bad1))
  expect_error(EqualizePiecewise(gim, N, smax = bad2))
  expect_error(EqualizePiecewise(gim, N, smin = bad1))
  expect_class(EqualizePiecewise(gim, N, smin = bad2), class_imager)
  expect_class(EqualizePiecewise(gim, N), class_imager)
})