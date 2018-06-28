#' Otsu method
#'
#' compute threshold value by Otsu method
#' @par im a grayscale image of class cimg
#' @par intervalnumber interval number  of histogram
#' @return double
#' @Reference Nobuyuki Otsu (1979) A threshold selection method from gray-level histograms. IEEE. 
#' @author Shota Ochi
threshold_otsu <- function(im, intervalnumber = 1000)
{
dimim <- dim(im)
ordered <- as.vector(im)
ordered <- ordered[order(ordered)]
minim <- ordered[1]
maxim <- ordered[length(ordered)]
bins <- seq(minim, maxim, length.out = intervalnumber + 1)
prob_otsu <- make_prob_otsu(ordered, bins, intervalnumber, dimim[1], dimim[2])
thresval <- get_th_otsu(prob_otsu, bins)
return(thresval)
}

#' Iterative Triclass Thresholding
#'
#' compute threshold value by Iterative Triclass Threshold Technique
#' @par im a grayscale image of class cimg
#' @par intervalnumber interval number  of histogram
#' @par stopval value to determine whether stop iteration of triclass thresholding or not. Note that if repeat is set, stop is ignored.
#' @par repeatnum number of repetition of triclass thresholding 
#' @par returnvalue if returnvalue is TRUE, ThresholdTriclass returns threshold value. if FALSE, ThresholdTriclass returns binary image.
#' @return a grayscale image of class cimg or threshold value
#' @Reference Cai HM, Yang Z, Cao XH, Xia WM, Xu XY (2014). A New Iterative Triclass Thresholding Technique in Image Segmentation. IEEE TRANSACTIONS ON IMAGE PROCESSING.
#' @author Shota Ochi
#' @export 
#' @example 
#' g <- grayscale(boats)
#' layout(matrix(1:4, 2, 2))
#' plot(boats, main = "Original")
#' plot(g, main = "Grayscale")
#' threshold(g) %>% plot(main = "Otsu")
#' ThresholdTriclass(g) %>% plot(main = "Triclass")
ThresholdTriclass <- function(im, intervalnumber = 1000, stopval = 0.1, repeatnum, returnvalue = FALSE)
{
  #sanity check of im ,intervalnumber, and returnvalue
  res_sanityecheck_im <- CheckSanityim(im)
  res_sanityecheck_intervalnumber <- CheckSanitypositivenumeric(intervalnumber, "intervalnumber")
  res_sanityecheck_returnvalue <- CheckSanitylogical(returnvalue, "returnvalue")
  if (!all(c(res_sanitycheck_im, res_sanityecheck_intervalnumber, res_sanityecheck_returnvalue))) 
  {
    return(NULL)
  }
  
  if (missing(repeatnum))
  { 
    #sanity check of stopval
	res_sanitycheck_stopval <- CheckSanitypositivenumeric(stopval, "stopval")
	if(!res_sanitycheck_stopval)
	{
	  return(NULL)
	}
	
    dimim <- dim(im)
    ordered <- as.vector(im)
    ordered <- ordered[order(ordered)]
    minim <- ordered[1]
    maxim <- ordered[length(ordered)]
    bins <- seq(minim, maxim, length.out = intervalnumber + 1)
    prob_otsu <- make_prob_otsu(ordered, bins, as.integer(intervalnumber), dimim[1], dimim[2])
    thresval <- get_th_otsu(prob_otsu, bins)
    thresval_pre <- thresval + 2 * stopval
    while (TRUE)
    {
	  indexf <- bins > thresval
	  indexb <- !indexf
      myu1 <- mean(bins[indexf])
	  myu0 <- mean(bins[indexb])
	  if (is.nan(myu0) || is.nan(myu1)) 
	  {
	    break
	  }
      indexTBD <- bins > myu0 & bins < myu1
	  bins <- bins[indexTBD]
	  prob_otsu <- prob_otsu[indexTBD]
	  thresval <- get_th_otsu(prob_otsu, bins)
	  if (abs(thresval - thresval_pre) < stopval) 
	  {
	    break
	  }
	  thresval_pre <- thresval
    }
  } else {
    #sanity check of repeatnum (repeatnum >= 1)
	res_sanitycheck_repeatnum <- CheckSanitypositivenumeric(repeatnum, "repeatnum")
	if(!res_sanitycheck_repeatnum)
	{
	  return(NULL)
	}
	
    dimim <- dim(im)
    ordered <- as.vector(im)
    ordered <- ordered[order(ordered)]
    minim <- ordered[1]
    maxim <- ordered[length(ordered)]
    bins <- seq(minim, maxim, length.out = intervalnumber + 1)
    prob_otsu <- make_prob_otsu(ordered, bins, intervalnumber, dimim[1], dimim[2])
    thresval <- get_th_otsu(prob_otsu, bins)
	for (i in seq_len(as.integer(repeatnum) - 1))
	{
	  indexf <- bins > thresval
	  indexb <- !indexf
      myu1 <- mean(bins[indexf])
	  myu0 <- mean(bins[indexb])
	  if (is.nan(myu0) || is.nan(myu1)) 
	  {
        message("Iteration was stopped in the middle because further iteration didn't bring anything.")          
	    break
	  }
      indexTBD <- bins > myu0 & bins < myu1
	  bins <- bins[indexTBD]
	  prob_otsu <- prob_otsu[indexTBD]
	  thresval <- get_th_otsu(prob_otsu, bins)
	} 
  }
  if(returnvalue) 
  {  
    return(thresval)
  }
  return(threshold(im, thresval))
}