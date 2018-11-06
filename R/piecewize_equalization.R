#' Piecewise Affine Histogram Equalization
#'
#' enhance contrast of image by piecewise affine histogram equalization
#' @param im a grayscale image of class cimg
#' @param N number of subintervals of partition. N controls how the input gray levels will be mapped in the output image.
#' if N is large, Piecewise Affine Equalization and Histogram Equalization are very similar.
#' @param smax maximum value of slopes. if smax is small, contrast enhancement is suppressed.   
#' @param smin minimum value of slopes. if smin is large, contrast enhancement is propelled, and saturations occur excessively.
#' @param range range of the pixel values of image. this function assumes that the range of pixel values of of an input image is [0,255] by default. you may prefer [0,1].
#' if you change range, you should change smax. one example is this (smax = range[2] - range[1]). 
#' @return a grayscale image of class cimg
#' @references Jose-Luis Lisani, Ana-Belen Petro, and Catalina Sbert, Color and Contrast Enhancement by Controlled Piecewise Affine Histogram Equalization, Image Processing On Line, 2 (2012), pp. 243-265. \url{https://doi.org/10.5201/ipol.2012.lps-pae}
#' @author Shota Ochi
#' @export
#' @examples
#' dev.new()
#' par(mfcol = c(1,2))
#' boats_g <- grayscale(boats)
#' plot(boats_g, main = "Original")
#' EqualizePiecewise(boats_g, 10) %>% plot(., main = "Piecewise Affine Equalization")
EqualizePiecewise <- function(im, N, smax = 255, smin = 0, range = c(0, 255))
{
  CheckSanityim(im)
  CheckSanityrange(range)
  dim_im <- dim(im)
  im <- as.vector(im)
  im_sorted <- im[order(im)]
  max_im <- max(im)
  min_im <- min(im)
  res <- piecewise_transformation(im, im_sorted, N, smax, smin, max_im, min_im, range[2], range[1])
  return(as.cimg(res, dim = dim_im))
}




