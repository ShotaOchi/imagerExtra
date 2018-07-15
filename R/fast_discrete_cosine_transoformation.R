#' Two Dimensional Discrete Cosine Transformation
#'
#' compute two dimensional discrete cosine transformation
#' @param imormat a grayscale image of class cimg or a numeric matrix
#' @param returnmat if returnmat is TRUE, DCT2D returns numeric matrix. if FALSE, DCT2D returns a grayscale image of class cimg.
#' @return a grayscale image of class cimg or a numeric matrix
#' @references Makhoul, J. (1980). A fast cosine transform in one and two dimensions. IEEE Transactions on Acoustics, Speech, and Signal Processing. 28 (1): 27-34.
#' @author Shota Ochi
#' @export
#' @examples 
#' g <- grayscale(boats)
#' DCT2D(g)
DCT2D <- function(imormat, returnmat = FALSE) {
  CheckSanityimormat(imormat)
  CheckSanitylogical(returnmat, "returnmat")
  if (is.cimg(imormat)) {
    imormat <- as.matrix(imormat)
  }
  temp <- DCT2D_reorder(imormat)
  temp <- fftw2d(temp)
  res <- DCT2D_fromDFT(temp)
  if (returnmat) {
    return(res)
  }
  return(as.cimg(res))
}

#' Two Dimensional Inverse Discrete Cosine Transformation
#'
#' compute two dimensional inverse discrete cosine transformation
#' @param imormat a grayscale image of class cimg or a numeric matrix
#' @param returnmat if returnmat is TRUE, IDCT2D returns numeric matrix. if FALSE, IDCT2D returns a grayscale image of class cimg.
#' @return a grayscale image of class cimg or a numeric matrix
#' @references Makhoul, J. (1980). A fast cosine transform in one and two dimensions. IEEE Transactions on Acoustics, Speech, and Signal Processing. 28 (1): 27-34.
#' @author Shota Ochi
#' @export
#' @examples 
#' g <- grayscale(boats)
#' layout(matrix(1:2, 1, 2))
#' plot(g, main = "Original")
#' gg <- DCT2D(g) %>% IDCT2D() %>% plot(main = "Transformed")
#' mean((g - gg)^2)
IDCT2D <- function(imormat, returnmat = FALSE) {
  CheckSanityimormat(imormat)
  CheckSanitylogical(returnmat, "returnmat")
  if (is.cimg(imormat)) {
    imormat <- as.matrix(imormat)
  }
  dimim <- dim(imormat)
  size <- dimim[1] * dimim[2]
  temp <- IDCT2D_toDFT(imormat)
  temp <- Re(fftw2d(temp, inverse = 1))
  res <- IDCT2D_retrievex(temp) / size
  if (returnmat) {
    return(res)
  }
  return(as.cimg(res))
}