#$' Convert string to numeric as percentile
#$'
#$' @param stringInput string that means a percentile
#$' @return numeric that means a percentile
#$' @author Shota Ochi
#$' @examples
#$' ConvertPercentile("1%") # return 1
ConvertPercentile <- function(stringInput)
{
    if (any(class(stringInput) != "character"))
	{
	    warning("stringInput must be a character.", call. = FALSE)
		return(NULL)
	}
    splitted <- strsplit(stringInput , "%")
	candidate <- splitted[[1]][1]
	if (length(candidate) == 0) 
	{
	    warning("saturation percentage parameters (s, sleft, or sright) are not appropriate.", call. = FALSE)
		return(NULL)
	}
	candidate <- suppressWarnings(as.numeric(candidate))
	if (is.na(candidate)) 
	{
		warning("saturation percentage parameters (s, sleft, or sright) are not appropriate.", call. = FALSE)
		return(NULL)
	}
    return(candidate)
}

CheckSanityim <- function(im)
{
    if (!any(class(im) == "cimg")) 
	{
	    warning("im must be a image of class cimg.", call. = FALSE)
		return(FALSE)
	}
	if (depth(im) != 1) 
	{
	    warning("the depth of im must be 1.", call. = FALSE)
		return(FALSE)
	}
	if (imager::spectrum(im) != 1) 
	{
	    warning("im must be a grayscale image.", call. = FALSE)
		return(FALSE)
	}
	if (any(is.na(im))) 
	{
	    warning("im has NA. NA is unacceptable.", call. = FALSE)
		return(FALSE)
	}
	if (!is.numeric(im))
	{
	    warning("im must be numeric.", call. = FALSE)
	    return(FALSE)
	}
	return(TRUE)
}

CheckSanityimcol <- function(imcol)
{
    if (!any(class(imcol) == "cimg")) 
	{
	    warning("imcol must be a image of class cimg.", call. = FALSE)
		return(FALSE)
	}
	if (depth(imcol) != 1) 
	{
	    warning("the depth of imcol must be 1.", call. = FALSE)
		return(FALSE)
	}
	if (imager::spectrum(imcol) != 3) 
	{
	    warning("imcol must be a color image.", call. = FALSE)
		return(FALSE)
	}
	if (any(is.na(imcol))) 
	{
	    warning("imcol has NA. NA is unacceptable.", call. = FALSE)
		return(FALSE)
	}
	if (!is.numeric(imcol))
	{
	    warning("imcol must be numeric.", call. = FALSE)
	    return(FALSE)
	}
	return(TRUE)
}

CheckSanityrange <- function(range)
{
	if (length(range) != 2)
	{
	    warning("the length of range must be 2.", call. = FALSE)
		return(FALSE)
	}
	if (any(is.na(range)))
	{
	    warning("range has NA. NA is unacceptable.", call. = FALSE)
		return(FALSE)
	}
	if (!is.numeric(range))
	{
	    warning("range must be a vector of numeric.", call. = FALSE)
		return(FALSE)
	}
	if (any(range < 0))
	{
	    warning("elements of range must be greater than or equal to 0.", call. = FALSE)
		return(FALSE)
	}
	if (range[1] > range[2])
	{
	    warning("range was ordered.", call. = FALSE)
		range_ordered <- range[order(range)]
		assign("range", range_ordered, pos = parent.frame())
	}
	return(TRUE)
}