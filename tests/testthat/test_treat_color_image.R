library(imagerExtra)

test_that("treat color image", 
{
    notim <- 1
    im <- boats
	gim <- grayscale(im)
	gim2 <- imrep(gim, 2) %>% imappend(., "z")
    im2 <- imrep(im, 2) %>% imappend(., "z")
 	expect_equal(Grayscale(notim), NULL)
    expect_warning(Grayscale(notim))   
 	expect_equal(Grayscale(gim), NULL)
    expect_warning(Grayscale(gim))
	expect_equal(Grayscale(im2), NULL)
    expect_warning(Grayscale(im2))

 	expect_equal(GetHue(notim), NULL)
    expect_warning(GetHue(notim))   
 	expect_equal(GetHue(gim), NULL)
    expect_warning(GetHue(gim))
	expect_equal(GetHue(im2), NULL)
    expect_warning(GetHue(im2))

 	expect_equal(RestoreHue(notim, im), NULL)
    expect_warning(RestoreHue(notim, im))   
 	expect_equal(RestoreHue(im, im), NULL)
    expect_warning(RestoreHue(im, im))
	expect_equal(RestoreHue(gim2, im), NULL)
    expect_warning(RestoreHue(gim2, im))
	expect_equal(RestoreHue(gim, notim), NULL)
    expect_warning(RestoreHue(gim, notim))
	expect_equal(RestoreHue(gim, im2), NULL)
    expect_warning(RestoreHue(gim, im2))	
})