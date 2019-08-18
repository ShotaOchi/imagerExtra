# check assert_im_px()

test_that("OCR",
{
  if (requireNamespace("tesseract", quietly = TRUE))
  {
    expect_error(OCR(gim_bad))
    expect_error(OCR(im_bad))
    expect_error(OCR(gim2pix))
    expect_error(OCR(gim_badpix))
    
    expect_error(OCR_data(gim_bad))
    expect_error(OCR_data(im_bad))
    expect_error(OCR_data(gim2pix))
    expect_error(OCR_data(gim_badpix))
  }
})
