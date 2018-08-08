#$' Otsu method
#$'
#$' compute threshold value by Otsu method
#$' @param im a grayscale image of class cimg
#$' @param intervalnumber interval number  of histogram
#$' @return double
#$' @references Nobuyuki Otsu (1979) A threshold selection method from gray-level histograms. IEEE. 
#$' @author Shota Ochi
#threshold_otsu <- function(im, intervalnumber = 1000)
#{
#dimim <- dim(im)
#ordered <- as.vector(im)
#ordered <- ordered[order(ordered)]
#minim <- ordered[1]
#maxim <- ordered[length(ordered)]
#bins <- seq(minim, maxim, length.out = intervalnumber + 1)
#prob_otsu <- make_prob_otsu(ordered, bins, intervalnumber, dimim[1], dimim[2])
#thresval <- get_th_otsu(prob_otsu, bins)
#return(thresval)
#}

#' Iterative Triclass Thresholding
#'
#' compute threshold value by Iterative Triclass Threshold Technique
#' @param im a grayscale image of class cimg
#' @param stopval value to determine whether stop iteration of triclass thresholding or not. Note that if repeat is set, stop is ignored.
#' @param repeatnum number of repetition of triclass thresholding
#' @param intervalnumber interval number  of histogram 
#' @param returnvalue if returnvalue is TRUE, ThresholdTriclass returns threshold value. if FALSE, ThresholdTriclass returns pixset.
#' @return a pixel set or a numeric
#' @references Cai HM, Yang Z, Cao XH, Xia WM, Xu XY (2014). A New Iterative Triclass Thresholding Technique in Image Segmentation. IEEE TRANSACTIONS ON IMAGE PROCESSING.
#' @author Shota Ochi
#' @export 
#' @examples 
#' g <- grayscale(boats)
#' layout(matrix(1:4, 2, 2))
#' plot(boats, main = "Original")
#' plot(g, main = "Grayscale")
#' threshold(g) %>% plot(main = "A Variant of Otsu")
#' ThresholdTriclass(g) %>% plot(main = "Triclass")
ThresholdTriclass <- function(im, stopval = 0.01, repeatnum, intervalnumber = 1000, returnvalue = FALSE)
{
  #sanity check of im ,intervalnumber, and returnvalue
  CheckSanityim(im)
  CheckSanitypositivenumeric(intervalnumber, "intervalnumber")
  CheckSanitylogical(returnvalue, "returnvalue")
  minim <- min(im)
  maxim <- max(im)
  if (minim == maxim) 
  {
    stop("im has only one unique value. ThresholdTriclass can't be applied for such a image.", call. = FALSE)
  }
  
  if (missing(repeatnum))
  { 
    #sanity check of stopval
    CheckSanitypositivenumeric(stopval, "stopval")

    dimim <- dim(im)
    ordered <- as.vector(im)
    ordered <- ordered[order(ordered)]
    bins <- seq(minim, maxim, length.out = intervalnumber + 1)
    prob_otsu <- make_prob_otsu(ordered, bins[2:length(bins)], as.integer(intervalnumber), dimim[1], dimim[2])
    bins <- (bins[2:length(bins)] + bins[1:(length(bins)-1)]) / 2
    thresval <- get_th_otsu(prob_otsu, bins)
    thresval_pre <- thresval + 2 * stopval
    while (TRUE)
    {
    indexf <- ordered > thresval
    indexb <- !indexf
    myu1 <- mean(ordered[indexf])
    myu0 <- mean(ordered[indexb])
    ordered <- ordered[ordered >= myu0 & ordered <= myu1]
    if (is.nan(myu0) || is.nan(myu1)) 
    {
      break
    }
      indexTBD <- bins >= myu0 & bins <= myu1
    bins <- bins[indexTBD]
    prob_otsu <- prob_otsu[indexTBD]
    if (sum(prob_otsu) == 0 || length(prob_otsu) < 2)
    {
      break
    }
    prob_otsu <- prob_otsu / sum(prob_otsu)	  
    thresval <- get_th_otsu(prob_otsu, bins)
    if (abs(thresval - thresval_pre) < stopval) 
    {
      break
    }
    thresval_pre <- thresval
    }
  } else {
    #sanity check of repeatnum (repeatnum >= 1)
    CheckSanitypositivenumeric(repeatnum, "repeatnum")

    dimim <- dim(im)
    ordered <- as.vector(im)
    ordered <- ordered[order(ordered)]
    bins <- seq(minim, maxim, length.out = intervalnumber + 1)
    prob_otsu <- make_prob_otsu(ordered, bins[2:length(bins)], intervalnumber, dimim[1], dimim[2])
    bins <- (bins[2:length(bins)] + bins[1:(length(bins)-1)]) / 2
    thresval <- get_th_otsu(prob_otsu, bins)
    for (i in seq_len(as.integer(repeatnum) - 1))
    {
    indexf <- ordered > thresval
    indexb <- !indexf
    myu1 <- mean(ordered[indexf])
    myu0 <- mean(ordered[indexb])
    ordered <- ordered[ordered >= myu0 & ordered <= myu1]
    if (is.nan(myu0) || is.nan(myu1)) 
    {
        message("Iteration was stopped in the middle.")          
      break
    }
    indexTBD <- bins >= myu0 & bins <= myu1
    bins <- bins[indexTBD]
    prob_otsu <- prob_otsu[indexTBD]
    if (sum(prob_otsu) == 0  || length(prob_otsu) < 2)
    {
      message("Iteration was stopped in the middle.")  
      break
    }
    prob_otsu <- prob_otsu / sum(prob_otsu)
    thresval <- get_th_otsu(prob_otsu, bins)
    } 
  }
  if (returnvalue) 
  {  
    return(thresval)
  }
  return(threshold(im, thresval))
}