#' Two Dimensional Discrete Cosine Transformation and Inverse Cosine Transformation
#'
#' DCT2D computes two dimensional discrete cosine transformation.
#' IDCT2D computes two dimensional inverse discrete cosine transformation.
#' @name DCT
#' @param imormat a grayscale image of class cimg or a numeric matrix
#' @param returnmat if returnmat is TRUE, returns numeric matrix. if FALSE, returns a grayscale image of class cimg.
#' @return a grayscale image of class cimg or a numeric matrix
#' @references Makhoul, J. (1980). A fast cosine transform in one and two dimensions. IEEE Transactions on Acoustics, Speech, and Signal Processing. 28 (1): 27-34.
#' @author Shota Ochi
#' @examples 
#' g <- grayscale(boats)
#' layout(matrix(1:2, 1, 2))
#' plot(g, main = "Original")
#' gg <- DCT2D(g) %>% IDCT2D() %>% plot(main = "Transformed")
#' mean((g - gg)^2)
NULL

#' @rdname DCT
#' @export 
DCT2D <- function(imormat, returnmat = FALSE) 
{
  CheckSanityimormat(imormat)
  CheckSanitylogical(returnmat, "returnmat")
  if (is.cimg(imormat)) 
  {
    imormat <- as.matrix(imormat)
  }
  temp <- DCT2D_reorder(imormat)
  temp <- fftw2d(temp)
  res <- DCT2D_fromDFT(temp)
  if (returnmat) 
  {
    return(res)
  }
  return(as.cimg(res))
}

#' @rdname DCT
#' @export 
IDCT2D <- function(imormat, returnmat = FALSE) 
{
  CheckSanityimormat(imormat)
  CheckSanitylogical(returnmat, "returnmat")
  if (is.cimg(imormat)) 
  {
    imormat <- as.matrix(imormat)
  }
  dimim <- dim(imormat)
  size <- dimim[1] * dimim[2]
  temp <- IDCT2D_toDFT(imormat)
  temp <- Re(fftw2d(temp, inverse = 1))
  res <- IDCT2D_retrievex(temp) / size
  if (returnmat) 
  {
    return(res)
  }
  return(as.cimg(res))
}