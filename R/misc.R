#' imagerExtra: Extra Image Processing Library Based on Imager
#'
#' imagerExtra is built on imager. imager by Simon Simon Barthelme provides an interface with CImg that is a C++ library for image processing. imager makes functions of CImg accessible from R and adds many utilities for accessing and working with image data from R.
#' imagerExtra provides several advanced functions based on imager.
#' @docType package
#' @name imagerExtra
NULL

#' @useDynLib imagerExtra, .registration=TRUE
#' @importFrom dtt mvdct
#' @importFrom imager add.color
#' @importFrom imager as.cimg
#' @importFrom imager at<-
#' @importFrom imager B
#' @importFrom imager B<-
#' @importFrom imager depth
#' @importFrom imager G
#' @importFrom imager G<-
#' @importFrom imager imfill
#' @importFrom imager R
#' @importFrom imager R<-
#' @importFrom imager spectrum
#' @importFrom imager where
#' @importFrom magrittr %>%
#' @importFrom Rcpp sourceCpp
NULL




