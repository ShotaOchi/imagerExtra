class_imager <- "cimg"

assert_im <- function(im)
{
  assert_class(im, class_imager)
  if (depth(im) != 1)
  {
    stop(sprintf("%s must be a grayscale image.", deparse(substitute(range))))
  }
  if (spectrum(im) != 1) 
  {
    stop(sprintf("%s must be a grayscale image.", deparse(substitute(range))))
  }
  if (any(is.na(im))) 
  {
    stop(sprintf("%s has NA. NA is unacceptable.", deparse(substitute(range))))
  }
}

assert_imcol <- function(imcol)
{
  assert_class(imcol, class_imager)
  if (depth(imcol) != 1)
  {
    stop(sprintf("%s must be an image of class cimg.", deparse(substitute(range))))
  }
  if (spectrum(imcol) != 3) 
  {
    stop(sprintf("%s number of color channels of imcol must be 3.", deparse(substitute(range))))
  }
  if (any(is.na(imcol))) 
  {
    stop(sprintf("%s has NA. NA is unacceptable.", deparse(substitute(range))))
  }
}

assert_range <- function(range)
{
  assert_numeric(range, lower = 0, finite = TRUE, any.missing = FALSE, sorted = TRUE, len = 2, .var.name = deparse(substitute(range)))
}

assert_positive_numeric_one_elem <- function(mynumeric)
{
  assert_numeric(mynumeric, finite = TRUE, any.missing = FALSE, len = 1, .var.name = deparse(substitute(mynumeric)))
  if (mynumeric <= 0)
  {
    stop(sprintf("%s must be greater than 0.", deparse(substitute(mynumeric))))
  }
}

assert_positive0_numeric_one_elem <- function(mynumeric)
{
  assert_numeric(mynumeric, lower = 0, finite = TRUE, any.missing = FALSE, len = 1, .var.name = deparse(substitute(mynumeric)))
}

assert_numeric_one_elem <- function(mynumeric)
{
  assert_numeric(mynumeric, finite = TRUE, any.missing = FALSE, len = 1, .var.name = deparse(substitute(mynumeric)))
}

assert_numeric_vec <- function(numericvec)
{
  assert_numeric(numericvec, finite = TRUE, any.missing = FALSE, min.len = 1, .var.name = deparse(substitute(numericvec)))
}

assert_logical_one_elem <- function(mylogical)
{
  assert_logical(mylogical, any.missing = FALSE, len = 1, .var.name = deparse(substitute(mylogical)))
}

assert_im_mat <- function(imormat)
{
  assert(check_class(imormat, class_imager), check_class(imormat, "matrix"), .var.name = deparse(substitute(imormat)))
  if (any(class(imormat) == class_imager))
  {
    assert_im(imormat)
  } else
  {
    assert(check_matrix(imormat, mode = "numeric", any.missing = FALSE),
           check_matrix(imormat, mode = "double", any.missing = FALSE),
           check_matrix(imormat, mode = "integer", any.missing = FALSE),
           .var.name = deparse(substitute(imormat)))
  }
}

assert_im_px <- function(imorpx)
{
  assert(check_class(imorpx, class_imager), check_class(imorpx, "pixset"), .var.name = deparse(substitute(imorpx)))
  imorpx <- as.cimg(imorpx)
  if (spectrum(imorpx) == 1)
  {
    assert_im(imorpx)
  } else
  {
    assert_imcol(imorpx)
  }
}

#$' Convert string to numeric as percentile
#$'
#$' @param s_input string that means a percentile
#$' @return numeric that means a percentile
#$' @examples
#$' convert_percentile("1%") # return 1
convert_percentile <- function(s_input)
{
  splitted <- strsplit(s_input , "%")
  candidate <- suppressWarnings(as.numeric(splitted[[1]][1]))
  if (!test_numeric(candidate, lower = 0, finite = TRUE, len = 1)) 
  {
    stop("saturation percentage parameter (s, sleft, or sright) is not appropriate.")
  }
  return(candidate)
}

assert_s <- function(s_input)
{
  assert(check_character(s_input, min.chars = 1, any.missing = FALSE, len = 1), check_numeric(s_input, lower = 0, finite = TRUE, len = 1), .var.name = deparse(substitute(s_input)))
  if (!is.numeric(s_input))
  {
    s_input <- convert_percentile(s_input)
  }
  return(s_input)
}

assert_s_left_right <- function(sleft, sright)
{
  if (sleft + sright > 100)
  {
    stop("Saturation parameters (s, sleft, or sright) are too large. Confirm the following condition is satisfied. s <= 50 or sleft + sright <= 100.")
  }
}

assert_char <- function(mychar)
{
  assert_character(mychar, min.chars = 1, any.missing = FALSE, len = 1, .var.name = deparse(substitute(s_input)))
}
