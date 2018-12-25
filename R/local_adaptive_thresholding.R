#' Local Adaptive Thresholding
#' 
#' @param im a grayscale image of class cimg
#' @param k a numeric in the range [0,1]. when k is high, local threshold values tend to be lower. when k is low, local threshold value tend to be higher.
#' @param windowsize windowsize controls the number of local neighborhood
#' @param range this function assumes that the range of pixel values of of input image is [0,255] by default. you may prefer [0,1]. 
#'        Note that range determines the max standard deviation. The max standard deviation plays an important role in this function.
#' @return a pixel set
#' @references Faisal Shafait, Daniel Keysers, Thomas M. Breuel, "Efficient implementation of local adaptive thresholding techniques using integral images", Proc. SPIE 6815, Document Recognition and Retrieval XV, 681510 (28 January 2008)
#' @author Shota Ochi
#' @export
#' @examples 
#' layout(matrix(1:4, 2, 2))
#' plot(papers, main = "Original")
#' threshold(papers) %>% plot(main = "A variant of Otsu")
#' ThresholdAdaptive(papers, 0, range = c(0,1)) %>% plot(main = "local adaptive (k = 0)")
#' ThresholdAdaptive(papers, 0.2, range = c(0,1)) %>% plot(main = "local adaptive (k = 0.2)")
ThresholdAdaptive <- function(im, k, windowsize = 17, range = c(0,255)) 
{
  CheckSanityim(im)
  CheckSanitypositive0numeric(k, "k")
  CheckSanitypositivenumeric(windowsize, "windowsize")
  CheckSanityrange(range)
  windowsize <- as.integer(windowsize)  
  if (windowsize <= 2) 
  {
    stop("windowsize must be greater than or equal to 3", call.=FALSE)  
  } 
  if (windowsize %% 2 == 0) 
  {
    warning(sprintf("windowsize is even (%d). windowsize will be treated as %d", windowsize, windowsize+1))
    windowsize <- as.integer(windowsize + 1)
  }
  if (windowsize >= width(im) || windowsize >= height(im)) 
  {
    stop("windowsize is too large.", call.=FALSE)
  }
  if (k > 1) 
  {
    stop("k is out of range. k must be in [0,1].", call.=FALSE)
  }
  
  maxsd <- (range[2] - range[1]) / 2
  if (maxsd == 0) 
  {
    stop("range[1] must not be same as range[2].", call.=FALSE)
  }
  
  res <- threshold_adaptive(as.matrix(im), k, windowsize, maxsd)
  return(as.pixset(as.cimg(res)))
}