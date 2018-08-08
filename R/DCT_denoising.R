#' denoise image by DCT denoising
#'
#' @param im a grayscale image of class cimg
#' @param sdn standard deviation of Gaussian white noise
#' @param flag_dct16x16 flag_dct16x16 determines the size of patches. if TRUE, the size of patches is 16x16. if FALSE, the size if patches is 8x8.
#' @return a grayscale image of class cimg
#' @references Guoshen Yu, and Guillermo Sapiro, DCT Image Denoising: a Simple and Effective Image Denoising Algorithm, Image Processing On Line, 1 (2011), pp. 292-296. \url{https://doi.org/10.5201/ipol.2011.ys-dct}
#' @author Shota Ochi
#' @export
#' @examples
#' dev.new()
#' par(mfcol = c(1,2))
#' boats_g <- grayscale(boats)
#' boats_noisy <- imnoise(dim = dim(boats_g), sd = 0.05) + boats_g 
#' plot(boats_noisy, main = "Noisy Boats")
#' DenoiseDCT(boats_g, 0.05) %>% plot(., main = "Denoised Boats")
DenoiseDCT <- function(im, sdn, flag_dct16x16 = FALSE)
{
    CheckSanityim(im)
    CheckSanitypositivenumeric(sdn, "sdn")
    CheckSanitylogical(flag_dct16x16, "flag_dct16x16")
    dim_im <- dim(im)
    res <- DCTdenoising(as.matrix(im), dim_im[2], dim_im[1], sdn, as.integer(!flag_dct16x16)) %>% as.cimg()
    return(res)
}