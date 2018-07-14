#' compute average of RGB channels
#'
#' @param imcol a color image of class cimg
#' @return a grayscale image of class cimg
#' @author Shota Ochi
#' @export
#' @examples
#' Grayscale(boats) %>% plot
Grayscale <- function(imcol) 
{
    CheckSanityimcol(imcol)
    (R(imcol) + G(imcol) + B(imcol)) / 3
}

#' store hue of color image
#'
#' @param imcol a color image of class cimg
#' @return a color image of class cimg
#' @author Shota Ochi
#' @export
#' @examples
#' GetHue(boats)
GetHue <- function(imcol) 
{
    CheckSanityimcol(imcol)
    res <- imfill(dim=dim(imcol)) %>% add.color
    sumRGB <- Grayscale(imcol)
    pixels0 <- where(sumRGB == 0)
    at(sumRGB, pixels0[,"x"], pixels0[,"y"]) <- 1
    R(res) <- R(imcol) / sumRGB
    G(res) <- G(imcol) / sumRGB
    B(res) <- B(imcol) / sumRGB
    return(res)
}

#' restore hue of color image
#'
#' @param im a grayscale image of class cimg
#' @param hueim a color image of class cimg
#' @return a color image of class cimg
#' @author Shota Ochi
#' @export
#' @examples
#' g <- Grayscale(boats)
#' hue <- GetHue(boats)
#' layout(matrix(1:2, 1, 2))
#' plot(g, main = "Original")
#' RestoreHue(g, hue) %>% plot(main="Resotred")
RestoreHue <- function(im, hueim) 
{
    CheckSanityim(im)
    CheckSanityimcol(hueim)
    res <- imfill(dim=dim(im)) %>% add.color
    R(res) <- im * R(hueim)
    G(res) <- im * G(hueim)
    B(res) <- im * B(hueim)
    return(res)
}