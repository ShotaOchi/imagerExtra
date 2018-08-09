#' Fuzzy Entropy Image Segmentation
#'
#' automatic fuzzy thresholding based on particle swarm optimization 
#' @param im a grayscale image of class cimg
#' @param n swarm size
#' @param maxiter maximum iterative time
#' @param omegamax maximum inertia weight
#' @param omegamin minimum inertia weight
#' @param c1 acceleration coefficient
#' @param c2 acceleration coefficient
#' @param mutrate rate of gaussian mutation
#' @param vmaxcoef coefficient of maximum velocity
#' @param intervalnumber interval number of histogram
#' @param returnvalue if returnvalue is TRUE, returns a threshold value. if FALSE, returns a pixel set.
#' @return a pixsel set or a numeric
#' @references Linyi Li, Deren Li (2008). Fuzzy entropy image segmentation based on particle swarm optimization. Progress in Natural Science.
#' @author Shota Ochi
#' @export
#' @examples
#' g <- grayscale(boats)
#' layout(matrix(1:2, 1, 2))
#' plot(g, main = "Original")
#' ThresholdFuzzy(g) %>% plot(main = "Fuzzy Thresholding")
ThresholdFuzzy <- function(im, n = 50, maxiter = 100, omegamax = 0.9, omegamin = 0.1, c1 = 2, c2 = 2, mutrate = 0.2, vmaxcoef = 0.1, intervalnumber = 1000, returnvalue = FALSE)
{
  CheckSanityim(im)
  CheckSanitypositivenumeric(n, "n")
  CheckSanitypositivenumeric(maxiter, "maxiter")
  CheckSanitypositivenumeric(omegamax, "omegamax")
  CheckSanitypositivenumeric(omegamin, "omegamin")
  CheckSanitypositivenumeric(c1, "c1")
  CheckSanitypositivenumeric(c2, "c2")
  CheckSanitypositivenumeric(mutrate, "mutrate") 
  CheckSanitypositivenumeric(vmaxcoef, "vmaxcoef")  
  CheckSanitypositivenumeric(intervalnumber, "intervalnumber")
  CheckSanitylogical(returnvalue, "returnvalue")

  minval <- min(im)
  maxval <- max(im)
  if (n < 1) 
  {
    stop("n must be greater than or equal to 1.", call.=FALSE)
  }
  if (omegamax >= 1)
  {
    stop("omegamax must be smaller than 1", call. = FALSE) 
  }
  if (omegamin >= omegamax)
  {
    stop("omegamin must be smaller than omegamax", call. = FALSE)
  }
  if (maxiter < 2)
  {
    stop("maxiter must be greater than or equal to 2.", call. = FALSE)
  }
  if (intervalnumber < 2)
  {
    stop("intervalnumber must be greater than or equal to 2.", call. = FALSE)
  }
  if (minval == maxval) 
  {
    stop("im has only one unique value. ThresholdFuzzy can't be applied for such a image.", call. = FALSE)
  }
  n <- as.integer(n)
  maxiter <- as.integer(maxiter)
  intervalnumber <- as.integer(intervalnumber)
  interval <- seq(minval, maxval, length.out = intervalnumber + 1)
  interval <- interval[2:length(interval)]
  vmax <- vmaxcoef * intervalnumber
  range_local_search <- as.integer(intervalnumber * 0.1 / 4)
  ordered <- as.vector(im)
  ordered <- ordered[order(ordered)]
  imhist <- make_histogram_fuzzy(ordered, interval)
  thresval <- fuzzy_threshold(imhist, interval, n, maxiter, omegamax, omegamin, c1, c2, mutrate, vmax, range_local_search)
  if (returnvalue)
  {
    return(thresval)
  }
  return(threshold(im, thresval))
}