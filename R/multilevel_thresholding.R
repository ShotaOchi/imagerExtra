#' Multilevel Thresholding (Maximum Entropy Based Artificial Bee Colony Thresholding)
#'
#' automatic multilevel thresholding based on Maximum Entropy Based Artificial Bee Colony Thresholding
#' @param im a grayscale image of class cimg
#' @param k the number of thresholds
#' @param sn population size
#' @param mcn maximum cycle number
#' @param limit abandonment criteria
#' @param intervalnumber interval number of histogram
#' @param returnvalue if returnvalue is TRUE, returns threshold values. if FALSE, returns a grayscale image of class cimg.
#' @return a grayscale image of class cimg or a numeric vector
#' @references Ming-HuwiHorng (2011). Multilevel thresholding selection based on the artificial bee colony algorithm for image segmentation. Expert Systems with Applications.
#' @author Shota Ochi
#' @export
#' @examples
#' g <- grayscale(boats)
#' ThresholdML(g, 2) %>% plot
ThresholdML <- function(im, k, sn = 30, mcn = 100, limit = 100, intervalnumber = 1000, returnvalue = FALSE)
{
  CheckSanityim(im)
  CheckSanitypositivenumeric(k, "k")
  CheckSanitypositivenumeric(sn, "sn")
  CheckSanitypositivenumeric(mcn, "mcn")
  CheckSanitypositivenumeric(limit, "limit")
  CheckSanitypositivenumeric(intervalnumber, "intervalnumber")
  CheckSanitylogical(returnvalue, "returnvalue")
  minval <- min(im)
  maxval <- max(im)
  if (k < 1)
  {
    stop("k must be greater than or equal to 1.", call. = FALSE)
  }
  if (sn < 2) 
  {
    stop("sn must be greater than or equal to 2.", call.=FALSE)
  }
  if (mcn < 1) 
  {
    stop("mcn must be greater than or equal to 1.", call.=FALSE)
  }
  if (limit < 1) 
  {
    stop("limit must be greater than or equal to 1.", call.=FALSE)
  }
  if (intervalnumber < 2) 
  {
    stop("intervalnumber must be greater than or equal to 2.", call.=FALSE)
  }
  if (minval == maxval) 
  {
    stop("im has only one unique value. ThresholdML can't be applied for such a image.", call. = FALSE)
  }
  intervalnumber <- as.integer(intervalnumber)
  interval <- seq(minval, maxval, length.out = intervalnumber + 1)
  ordered <- as.vector(im)
  ordered <- ordered[order(ordered)]
  im_density <- make_density_multilevel(ordered, interval[2:length(interval)])
  im_integral_density <- make_integral_density_multilevel(im_density)
  idx_thresvals <- get_threshold_multilevel(im_density, im_integral_density, as.integer(k), as.integer(sn), as.integer(mcn), as.integer(limit))
  interval <- (interval[1:length(interval)-1] + interval[2:length(interval)]) / 2
  thresvals <- interval[idx_thresvals]
  if (returnvalue)
  {
    return(thresvals)
  }
  return(as.cimg(threshold_multilevel(as.matrix(im), thresvals)))
}