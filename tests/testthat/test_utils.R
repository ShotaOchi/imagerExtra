library(imagerExtra)

test_that("utils", 
{
    expect_error(ConvertPercentile(1))
	expect_error(ConvertPercentile(NULL))
    expect_equal(ConvertPercentile("1%"), 1)
    expect_equal(ConvertPercentile("50%"), 50)
	expect_error(ConvertPercentile("A%"))
	expect_error(ConvertPercentile("%"))
	
	notim <- 1
    im <- boats
    im2 <- imrep(im, 2) %>% imappend(., "z")
	gim <- grayscale(im)
	gim2 <- imrep(gim, 2) %>% imappend(., "z")
    N <- 3
	im_NA <- as.cimg(matrix(NA, 100, 100))
	im_char <- as.cimg(matrix("A", 100, 100))
	
	expect_error(CheckSanityim(notim))
	expect_error(CheckSanityim(gim2))
	expect_error(CheckSanityim(im))
	expect_error(CheckSanityim(im_NA))
	expect_error(CheckSanityim(im_char))
	expect_equal(CheckSanityim(gim), TRUE)
	
	expect_error(CheckSanityimcol(notim))
	expect_error(CheckSanityimcol(im2))
	expect_error(CheckSanityimcol(gim))
	expect_error(CheckSanityimcol(im_NA))
	expect_error(CheckSanityimcol(im_char))
	expect_equal(CheckSanityimcol(im), TRUE)
	
	range_bad1 <- c(1,1,1)
	range_bad2 <- c(-1,1)
	range_bad3 <- c(0, NA)
	range_bad4 <- c("A", 255)
	range_badorder <- c(255, 0)
	range_good <- c(0, 255)
	expect_error(CheckSanityrange(range_bad1))
	expect_error(CheckSanityrange(range_bad2))
	expect_error(CheckSanityrange(range_bad3))
	expect_error(CheckSanityrange(range_bad4))
	expect_equal(CheckSanityrange(range_badorder), TRUE)
    expect_warning(CheckSanityrange(range_badorder))
	expect_equal(CheckSanityrange(range_good), TRUE)
	
	numeric_bad1 <- c(1,2)
	numeric_bad2 <- NA
	numeric_bad3 <- NaN
	numeric_bad4 <- "A"
	numeric_bad5 <- -1
	numeric_good <- 1
    numeric_zero <- 0
	expect_error(CheckSanitypositivenumeric(numeric_bad1))
	expect_error(CheckSanitypositivenumeric(numeric_bad2))	
	expect_error(CheckSanitypositivenumeric(numeric_bad3))
	expect_error(CheckSanitypositivenumeric(numeric_bad4))
	expect_error(CheckSanitypositivenumeric(numeric_bad5))
    expect_error(CheckSanitypositivenumeric(numeric_zero))
	expect_equal(CheckSanitypositivenumeric(numeric_good), TRUE)

	expect_error(CheckSanitypositive0numeric(numeric_bad1))
	expect_error(CheckSanitypositive0numeric(numeric_bad2))	
	expect_error(CheckSanitypositive0numeric(numeric_bad3))
	expect_error(CheckSanitypositive0numeric(numeric_bad4))
	expect_error(CheckSanitypositive0numeric(numeric_bad5))
    expect_equal(CheckSanitypositive0numeric(numeric_zero), TRUE)
	expect_equal(CheckSanitypositive0numeric(numeric_good), TRUE)	    
	
	logical_bad1 <- c(TRUE, FALSE)
	logical_bad2 <- NA
	logical_bad3 <- 1
	logical_good <- TRUE
	expect_error(CheckSanitylogical(logical_bad1))
	expect_error(CheckSanitylogical(logical_bad2))
	expect_error(CheckSanitylogical(logical_bad3))
	expect_equal(CheckSanitylogical(logical_good), TRUE)
})