test_that("DCTdenoising",
{
  sdn_c <- 0.1
  sdn_bad1 <- NA
  
  flag_bad1 <- NA
  
  expect_error(DenoiseDCT(gim_bad, sdn_c))
  
  expect_error(DenoiseDCT(gim, sdn_bad1))
  
  expect_error(DenoiseDCT(gim, sdn_c, flag_dct16x16 = flag_bad1))
  
  expect_class(DenoiseDCT(gim, sdn_c), class_imager)
})