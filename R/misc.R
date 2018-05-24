#' imagerExtra: Extra Image Processing Library Based on Imager
#'
#' imagerExtra is built on imager. imager by Simon Simon Barthelme provides an interface with CImg that is a C++ library for image processing. imager makes functions of CImg accessible from R and adds many utilities for accessing and working with image data from R.
#' imagerExtra provides several advanced functions based on imager.
#' @docType package
#' @name imagerExtra
NULL

#' @useDynLib imagerExtra, .registration=TRUE
#' @importFrom imager as.cimg
#' @importFrom imager depth
#' @importFrom imager spectrum
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr %>%
#' @importFrom dtt mvdct
NULL




