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
  assert_im(im)
  assert_positive_numeric_one_elem(n)
  assert_positive_numeric_one_elem(maxiter)
  assert_positive_numeric_one_elem(omegamax)
  assert_positive_numeric_one_elem(omegamin)
  assert_positive_numeric_one_elem(c1)
  assert_positive_numeric_one_elem(c2)
  assert_positive_numeric_one_elem(mutrate) 
  assert_positive_numeric_one_elem(vmaxcoef)  
  assert_positive_numeric_one_elem(intervalnumber)
  assert_logical_one_elem(returnvalue)
  minval <- min(im)
  maxval <- max(im)
  if (n < 1) 
  {
    stop("n must be greater than or equal to 1.")
  }
  if (omegamax >= 1)
  {
    stop("omegamax must be smaller than 1") 
  }
  if (omegamin >= omegamax)
  {
    stop("omegamin must be smaller than omegamax")
  }
  if (maxiter < 2)
  {
    stop("maxiter must be greater than or equal to 2.")
  }
  if (intervalnumber < 2)
  {
    stop("intervalnumber must be greater than or equal to 2.")
  }
  if (minval == maxval) 
  {
    stop("im has only one unique value. ThresholdFuzzy can't be applied for such a image.")
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