library(imagerExtra)

test_that("piecewise_equalization", 
{
    notim <- 1
    im <- boats
	gim <- grayscale(im)
	gim2 <- imrep(gim, 2) %>% imappend(., "z")
    N <- 3
	range_bad1 <- c(1,1,1)
	range_bad2 <- c(-1,1)
	range_bad3 <- c(NA, 255)
	range_badorder <- c(255, 0)
	expect_equal(EqualizePiecewise(notim, N), NULL)
	expect_equal(EqualizePiecewise(gim2, N), NULL)
    expect_equal(EqualizePiecewise(im, N), NULL)
    expect_equal(EqualizePiecewise(gim, N, range = range_bad1), NULL)
	expect_equal(EqualizePiecewise(gim, N, range = range_bad2), NULL)
	expect_equal(EqualizePiecewise(gim, N, range = range_bad3), NULL)
    expect_warning(EqualizePiecewise(gim, N, range = range_badorder))
    expect_equal(EqualizePiecewise(gim, N), EqualizePiecewise(gim, N, range = range_badorder))
})