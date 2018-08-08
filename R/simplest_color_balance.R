#' Balance color of image by Simplest Color Balance
#'
#' @param im a grayscale image of class cimg
#' @param sleft left saturation percentage. sleft can be specified by numeric or string, e.g. 1 and "1\%". note that sleft is a percentile.
#' @param sright right saturation percentage. sright can be specified by numeric or string. note that sright is a percentile.
#' @param range this function assumes that the range of pixel values of of input image is [0,255] by default. you may prefer [0,1].
#' @return a grayscale image of class cimg
#' @references Nicolas Limare, Jose-Luis Lisani, Jean-Michel Morel, Ana Belen Petro, and Catalina Sbert, Simplest Color Balance, Image Processing On Line, 1 (2011), pp. 297-315. \url{https://doi.org/10.5201/ipol.2011.llmps-scb}
#' @author Shota Ochi
#' @export
#' @examples
#' dev.new()
#' par(mfcol = c(1,2))
#' boats_g <- grayscale(boats)
#' plot(boats_g, main = "Original")
#' BalanceSimplest(boats_g, 1, 1) %>% plot(., main = "Simplest Color Balance")
BalanceSimplest <- function(im, sleft, sright, range = c(0,255))
{
  CheckSanityim(im)
  CheckSanityrange(range)
  if (!is.numeric(sleft)) sleft <- ConvertPercentile(sleft)
  if (!is.numeric(sright)) sright <- ConvertPercentile(sright)
  if (is.null(sleft) || is.null(sright)) return(NULL)
  CheckSanitypositive0numeric(sleft, "s, sleft, or sright")
  CheckSanitypositive0numeric(sright, "s, sleft, or sright")
  if (sleft + sright > 100)
  {
    stop("saturation parameters (s, sleft, or sright) are too large. confirm the following condition is satisfied. s <= 50 or sleft + sright <= 100.", call. = FALSE)
  }
  dim_im <- dim(im)
  im <- as.vector(im)
  im_ordered <- im[order(im)]
  size_im <- length(im)
  end_left <- as.integer(sleft / 100 * size_im + 1) 
  end_right <- as.integer((100 - sright) / 100 * size_im)
  min_im <- im_ordered[end_left]
  max_im <- im_ordered[end_right]
  res <- saturateim(im, max_im, min_im, range[2], range[1])
  return(as.cimg(res, dim = dim_im))
}