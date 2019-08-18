# check assert_imcol()

test_that("treat color image", 
{
  expect_error(Grayscale(gim))
  expect_error(Grayscale(im2))
  expect_error(Grayscale(im_c2))
  expect_error(Grayscale(im_bad))
  expect_class(Grayscale(im), class_imager)

  expect_error(GetHue(im_bad))
  expect_class(GetHue(im), class_imager)

  expect_error(RestoreHue(gim_bad, im))
  expect_error(RestoreHue(gim, gim_bad))
  expect_class(RestoreHue(gim, im), class_imager)
})
