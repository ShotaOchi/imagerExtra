#' imagerExtra: Extra Image Processing Library Based on Imager
#'
#' imagerExtra is built on imager. imager by Simon Simon Barthelme provides an interface with CImg that is a C++ library for image processing. imager makes functions of CImg accessible from R and adds many utilities for accessing and working with image data from R.
#' imagerExtra provides advanced functions for image processing based on imager.
#' @docType package
#' @name imagerExtra
NULL

#' @useDynLib imagerExtra, .registration=TRUE
#' @importFrom checkmate assert
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_logical
#' @importFrom checkmate assert_numeric
#' @importFrom checkmate check_character
#' @importFrom checkmate check_class
#' @importFrom checkmate check_matrix
#' @importFrom checkmate check_numeric
#' @importFrom checkmate test_numeric
#' @importFrom fftwtools fftw2d
#' @importFrom imager add.color
#' @importFrom imager as.cimg
#' @importFrom imager as.pixset
#' @importFrom imager at<-
#' @importFrom imager B
#' @importFrom imager B<-
#' @importFrom imager depth
#' @importFrom imager G
#' @importFrom imager G<-
#' @importFrom imager grabRect
#' @importFrom imager height
#' @importFrom imager imfill
#' @importFrom imager is.cimg
#' @importFrom imager is.pixset
#' @importFrom imager R
#' @importFrom imager R<-
#' @importFrom imager save.image
#' @importFrom imager spectrum
#' @importFrom imager threshold
#' @importFrom imager where
#' @importFrom imager width
#' @importFrom magrittr %>%
#' @importFrom Rcpp sourceCpp
NULL
