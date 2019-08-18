test_that("fast discrete cosine transformation",
{
  mat_gim <- as.matrix(gim)
  mat_bad1 <- 1
  mat_bad2 <- NULL
  mat_bad3 <- matrix(NA,100,100)
  mat_bad4 <- matrix("A", 200,300)
  
  bad1 <- NA
  expect_error(DCT2D(gim_bad))

  expect_error(DCT2D(mat_bad1))
  expect_error(DCT2D(mat_bad2))
  expect_error(DCT2D(mat_bad3))
  expect_error(DCT2D(mat_bad4))
  
  expect_error(DCT2D(gim, returnmat = bad1))
  
  expect_equal(DCT2D(gim), DCT2D(mat_gim))
  expect_equal(DCT2D(gim, returnmat = TRUE), as.matrix(DCT2D(gim)))
  expect_class(DCT2D(gim), class_imager)
  expect_class(DCT2D(gim, returnmat = TRUE), "matrix")
  
  expect_error(IDCT2D(mat_bad1))
  expect_error(IDCT2D(mat_bad2))
  expect_error(IDCT2D(mat_bad3))
  expect_error(IDCT2D(mat_bad4))
  
  expect_error(IDCT2D(gim, returnmat = bad1))
  
  expect_equal(IDCT2D(gim), IDCT2D(mat_gim))
  expect_equal(IDCT2D(gim, returnmat = TRUE), as.matrix(IDCT2D(gim)))
  expect_class(IDCT2D(gim), class_imager)
  expect_class(IDCT2D(gim, returnmat = TRUE), "matrix")
})