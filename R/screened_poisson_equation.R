#' Correct inhomogeneous background of image by solving Screened Poisson Equation
#'
#' @param im a grayscale image of class cimg
#' @param lamda this function corrects inhomogeneous background while preserving image details. lamda controls the trade-off. when lamda is too large, this function acts as an edge detector.
#' @param s saturation percentage. this function uses \code{\link{BalanceSimplest}}. s is used as both sleft and sright. that's why s can not be over 50\%.
#' @param range this function assumes that the range of pixel values of of an input image is [0,255] by default. you may prefer [0,1].
#' @return a grayscale image of class cimg
#' @references Jean-Michel Morel, Ana-Belen Petro, and Catalina Sbert, Screened Poisson Equation for Image Contrast Enhancement, Image Processing On Line, 4 (2014), pp. 16-29. \url{https://doi.org/10.5201/ipol.2014.84}
#' @author Shota Ochi
#' @export
#' @examples
#' dev.new()
#' par(mfcol = c(1,2))
#' boats_g <- grayscale(boats)
#' plot(boats_g, main = "Original")
#' SPE(boats_g, 0.1) %>% plot(main = "Screened Poisson Equation")
SPE <- function(im, lamda, s = 0.1, range = c(0, 255))
{
  CheckSanityim(im)
  CheckSanityrange(range)
  CheckSanitypositivenumeric(lamda)
  im <- BalanceSimplest(im, s, s, range)
  im_dct <- DCT2D(as.matrix(im), returnmat = TRUE)
  im_dct_spe <- screened_poisson_dct(im_dct, lamda)
  im_corrected <- IDCT2D(im_dct_spe) %>% BalanceSimplest(s, s, range)
  return(im_corrected)
}