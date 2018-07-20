#' imagerExtra: Extra Image Processing Library Based on Imager
#'
#' imagerExtra is built on imager. imager by Simon Simon Barthelme provides an interface with CImg that is a C++ library for image processing. imager makes functions of CImg accessible from R and adds many utilities for accessing and working with image data from R.
#' imagerExtra provides several advanced functions based on imager.
#' @docType package
#' @name imagerExtra
NULL

#' @useDynLib imagerExtra, .registration=TRUE
#' @importFrom fftwtools fftw2d
#' @importFrom imager add.color
#' @importFrom imager as.cimg
#' @importFrom imager at<-
#' @importFrom imager B
#' @importFrom imager B<-
#' @importFrom imager depth
#' @importFrom imager G
#' @importFrom imager G<-
#' @importFrom imager imfill
#' @importFrom imager is.cimg
#' @importFrom imager R
#' @importFrom imager R<-
#' @importFrom imager spectrum
#' @importFrom imager threshold
#' @importFrom imager where
#' @importFrom magrittr %>%
#' @importFrom Rcpp sourceCpp
NULL

#' Photograph of a dog from GAHAG
#'
#' This photograph was downloaded from http://gahag.net/img/201603/03s/gahag-0062116383-1.jpg.
#' Its size was reduced by half to speed up loading and save space.
#' @format an image of class cimg
#' @source \url{http://gahag.net/img/201603/03s/gahag-0062116383-1.jpg}
"dogs"

#' Photograph of a paper
#' 
#' This photograph was filmed by Shota Ochi.
#' @format an image of class cimg
"papers"