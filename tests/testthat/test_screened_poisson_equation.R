test_that("screened_poisson_equation",
{
  s_c <- 0.1
  s_bad1 <- NA
  
  expect_error(SPE(gim_bad, s_c))
  
  expect_error(SPE(gim, s_bad1))
  expect_class(SPE(gim, 0), class_imager)
  
  expect_error(SPE(gim, s_c, s_bad1))
  expect_class(SPE(gim, s_c, 0), class_imager)
  
  expect_error(SPE(gim, s_c, range = range_bad1))
  
  expect_class(SPE(gim, s_c), class_imager)
})