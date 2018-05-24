library(imagerExtra)

test_that("DCTdenoising",
{
    notim <- 1
    im <- boats
	gim <- grayscale(im)
	gim2 <- imrep(gim, 2) %>% imappend(., "z")
    N <- 3
	sdn_c <- 0.1
	sdn_bad1 <- "A"
	sdn_bad2 <- c(1, 1)
	sdn_bad3 <- NA
	sdn_bad4 <- NULL
	sdn_bad5 <- -1
	flag_bad1 <- 1
	flag_bad2 <- NA
	expect_equal(DenoiseDCT(notim, sdn_c), NULL)
    expect_warning(DenoiseDCT(notim, sdn_c))
	expect_equal(DenoiseDCT(gim2, sdn_c), NULL)
	expect_warning(DenoiseDCT(gim2, s_c))
    expect_equal(DenoiseDCT(im, s_c), NULL)
	expect_warning(DenoiseDCT(im, s_c))
	expect_equal(DenoiseDCT(gim, sdn_bad1), NULL)
    expect_warning(DenoiseDCT(gim, sdn_bad1))
	expect_equal(DenoiseDCT(gim, sdn_bad2), NULL)
    expect_warning(DenoiseDCT(gim, sdn_bad2))
	expect_equal(DenoiseDCT(gim, sdn_bad3), NULL)
    expect_warning(DenoiseDCT(gim, sdn_bad3))
	expect_equal(DenoiseDCT(gim, sdn_bad4), NULL)
    expect_warning(DenoiseDCT(gim, sdn_bad4))
	expect_equal(DenoiseDCT(gim, sdn_bad5), NULL)
    expect_warning(DenoiseDCT(gim, sdn_bad5))
	expect_equal(DenoiseDCT(gim, sdn_c, flag_dct16x16 = flag_bad1), NULL)
    expect_warning(DenoiseDCT(gim, sdn_c, flag_dct16x16 = flag_bad1))
	expect_equal(DenoiseDCT(gim, sdn_c, flag_dct16x16 = flag_bad2), NULL)
    expect_warning(DenoiseDCT(gim, sdn_c, flag_dct16x16 = flag_bad2))
})