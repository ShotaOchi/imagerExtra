% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/screened_poisson_equation.R
\name{SPE}
\alias{SPE}
\title{Correct inhomogeneous background of image by solving Screened Poisson Equation}
\usage{
SPE(im, lamda, s = 0.1, range = c(0, 255))
}
\arguments{
\item{im}{a grayscale image of class cimg}

\item{lamda}{this function corrects inhomogeneous background while preserving image details. lamda controls the trade-off. when lamda is too large, this function acts as an edge detector.}

\item{s}{saturation percentage. this function uses \code{\link{BalanceSimplest}}. s is used as both sleft and sright. that's why s can not be over 50\%.}

\item{range}{this function assumes that the range of pixel values of of an input image is [0,255] by default. you may prefer [0,1].}
}
\value{
a grayscale image of class cimg
}
\description{
Correct inhomogeneous background of image by solving Screened Poisson Equation
}
\examples{
dev.new()
par(mfcol = c(1,2))
boats_g <- grayscale(boats)
plot(boats_g, main = "Original")
SPE(boats_g, 0.1) \%>\% plot(main = "Screened Poisson Equation")
}
\references{
Jean-Michel Morel, Ana-Belen Petro, and Catalina Sbert, Screened Poisson Equation for Image Contrast Enhancement, Image Processing On Line, 4 (2014), pp. 16-29. \doi{10.5201/ipol.2014.84}
}
\author{
Shota Ochi
}
