#' Double Plateaus Histogram Equalization
#'
#' enhance contrast of image by double plateaus histogram equalization.
#' @param im a grayscale image of class cimg
#' @param t_down lower threshold
#' @param t_up upper threshold
#' @param N the number of subintervals of histogram
#' @param range range of the pixel values of image. this function assumes that the range of pixel values of of an input image is [0,255] by default. you may prefer [0,1].
#' @return a grayscale image of class cimg
#' @references  Kun Liang, Yong Ma, Yue Xie, Bo Zhou ,Rui Wang (2012). A new adaptive contrast enhancement algorithm for infrared images based on double plateaus histogram equalization. Infrared Phys. Technol. 55, 309-315.
#' @author Shota Ochi
#' @export
#' @examples
#' g <- grayscale(dogs)
#' layout(matrix(1:2, 1, 2))
#' plot(g, main = "Original")
#' EqualizeDP(g, 20, 186) %>% plot(main = "Contrast Enhanced")
EqualizeDP <- function(im, t_down, t_up, N = 1000, range = c(0,255))
{
  CheckSanityim(im)
  CheckSanitynumeric(t_down, "t_down")
  CheckSanitynumeric(t_up, "t_up")
  CheckSanitypositivenumeric(N, "N")
  CheckSanityrange(range)
  if (t_down > t_up)
  {
    tmp_param <- t_up
    t_up <- t_down
    t_down <- tmp_param
    warning("The values of t_down and t_up were swapped because t_down was bigger than t_up.", call. = FALSE)
  }
  if (N < 2)
  {
    stop("N must be greater than or equal to 2.", call. = FALSE)
  }
  dim_im <- dim(im)
  minval <- min(im)
  maxval <- max(im)
  if (minval == maxval) 
  {
    stop("im has only one unique value. ThresholdFuzzy can't be applied for such a image.", call. = FALSE)
  }
  N <- as.integer(N)
  interval <- seq(minval, maxval, length.out = N + 1)
  interval1 <- interval[1:(length(interval)-1)]
  interval2 <- interval[2:length(interval)]
  ordered <- as.vector(im)
  ordered <- ordered[order(ordered)]
  imhist <- make_histogram_ADPHE(ordered, interval2)
  imhist_modified <- modify_histogram_ADPHE(imhist, t_down, t_up)
  res <- histogram_equalization_ADPHE(as.matrix(im), interval2, imhist_modified, range[1], range[2])
  return(as.cimg(res))
}

#' Adaptive Double Plateaus Histogram Equalization
#' 
#' compute the paramters, t_down and t_up, and then apply double plateaus histogram equalization.
#' @param im a grayscale image of class cimg
#' @param n window size to determine local maximum
#' @param N the number of subintervals of histogram
#' @param range range of the pixel values of image. this function assumes that the range of pixel values of of an input image is [0,255] by default. you may prefer [0,1].
#' @param returnparam if returnparam is TRUE, returns the computed parameters: t_down and t_up.
#' @return a grayscale image of class cimg or a numericvector
#' @references Kun Liang, Yong Ma, Yue Xie, Bo Zhou ,Rui Wang (2012). A new adaptive contrast enhancement algorithm for infrared images based on double plateaus histogram equalization. Infrared Phys. Technol. 55, 309-315.
#' @author Shota Ochi
#' @export
#' @examples
#' g <- grayscale(dogs)
#' layout(matrix(1:2, 1, 2))
#' plot(g, main = "Original")
#' EqualizeADP(g) %>% plot(main = "Contrast Enhanced")
EqualizeADP <- function(im, n = 5, N = 1000, range = c(0,255), returnparam = FALSE)
{
  CheckSanityim(im)
  CheckSanitypositivenumeric(n, "n")
  CheckSanitypositivenumeric(N, "N")
  CheckSanityrange(range)
  dim_im <- dim(im)
  minval <- min(im)
  maxval <- max(im)
  if (n < 3)
  {
    stop("n must be greater than or equal to 3.", call. = FALSE)
  }
  if (N < 2)
  {
    stop("N must be greater than or equal to 2.", call. = FALSE)
  }
  n <- as.integer(n)
  N <- as.integer(N)
  if (n %% 2 != 1)
  {
    n <- as.integer(n - 1)
    warning(sprintf("n is %d. n will be used as %d because n must be odd.", n+1, n), call. = FALSE)
  }
  if (minval == maxval) 
  {
    stop("im has only one unique value. ThresholdFuzzy can't be applied for such a image.", call. = FALSE)
  }
  N <- as.integer(N)
  interval <- seq(minval, maxval, length.out = N + 1)
  interval1 <- interval[1:(length(interval)-1)]
  interval2 <- interval[2:length(interval)]
  ordered <- as.vector(im)
  ordered <- ordered[order(ordered)]
  imhist <- make_histogram_ADPHE(ordered, interval2)
  idx_imhist_not0 <- imhist != 0
  imhist_not0 <- imhist[idx_imhist_not0]
  local_maximums <- find_local_maximum_ADPHE(imhist_not0, n)
  if (length(local_maximums) == 0)
  {
    warning("There is no local maximum in the histogram with zero statistics removed.\nTry to decrease n or increase N.", call. = FALSE)
    if (returnparam)
    {
      return(c(t_down = NA, t_up = NA))
    } else
    {
      return(im)
    }
  }
  t_up <- mean(local_maximums)
  d_min <- (range[2] - range[1]) / N #minimum gray level interval in modified histogram
  n_total <- dim_im[1] * dim_im[2]
  L <- length(imhist_not0)
  Sta <- min(n_total, t_up * L)
  M <- N
  t_down <- d_min * Sta / M
  if (returnparam)
  {
    return(c(t_down = t_down, t_up = t_up))
  }
  if (t_down > t_up)
  {
    tmp_param <- t_up
    t_up <- t_down
    t_down <- tmp_param
    warning("The values of t_down and t_up were swapped because t_down was bigger than t_up.", call. = FALSE)
  }
  imhist_modified <- modify_histogram_ADPHE(imhist, t_down, t_up)
  res <- histogram_equalization_ADPHE(as.matrix(im), interval2, imhist_modified, range[1], range[2])
  return(as.cimg(res))
}