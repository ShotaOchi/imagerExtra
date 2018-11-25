#$' Convert string to numeric as percentile
#$'
#$' @param stringInput string that means a percentile
#$' @return numeric that means a percentile
#$' @examples
#$' ConvertPercentile("1%") # return 1
ConvertPercentile <- function(stringInput)
{
    if (!is.character(stringInput))
    {
        stop("saturation percentage parameter (s, sleft, or sright) must be character or numeric.", call. = FALSE)
    }
    splitted <- strsplit(stringInput , "%")
    candidate <- splitted[[1]][1]
    if (length(candidate) == 0) 
    {
        stop("saturation percentage parameter (s, sleft, or sright) is not appropriate.", call. = FALSE)
    }
    candidate <- suppressWarnings(as.numeric(candidate))
    if (is.na(candidate)) 
    {
        stop("saturation percentage parameter (s, sleft, or sright) are not appropriate.", call. = FALSE)
    }
    return(candidate)
}

CheckSanityim <- function(im)
{
    if (!is.cimg(im)) 
    {
        stop("im must be a grayscale image of class cimg.", call. = FALSE)
    }
    if (depth(im) != 1) 
    {
        stop("the depth of im must be 1.", call. = FALSE)
    }
  if (imager::spectrum(im) != 1) 
  {
      stop("im must be a grayscale image.", call. = FALSE)
  }
  if (any(is.na(im))) 
  {
      stop("im has NA. NA is unacceptable.", call. = FALSE)
  }
  if (!is.numeric(im))
  {
      stop("im must be numeric.", call. = FALSE)
  }
  return(invisible(TRUE))
}

CheckSanityimcol <- function(imcol)
{
  if (!is.cimg(imcol)) 
  {
      stop("imcol must be an image of class cimg.", call. = FALSE)
  }
  if (depth(imcol) != 1) 
  {
    stop("the depth of imcol must be 1.", call. = FALSE)
  }
  if (imager::spectrum(imcol) != 3) 
  {
    stop("imcol must be a color image.", call. = FALSE)
  }
  if (any(is.na(imcol))) 
  {
    stop("imcol has NA. NA is unacceptable.", call. = FALSE)
  }
  if (!is.numeric(imcol))
  {
    stop("imcol must be numeric.", call. = FALSE)
  }
  return(invisible(TRUE))
}

CheckSanityrange <- function(range)
{
  if (length(range) != 2)
  {
    stop("the length of range must be 2.", call. = FALSE)
  }
  if (any(is.na(range)))
  {
    stop("range has NA. NA is unacceptable.", call. = FALSE)
  }
  if (!is.numeric(range))
  {
    stop("range must be a vector of numeric.", call. = FALSE)
  }
  if (any(range < 0))
  {
    stop("elements of range must be greater than or equal to 0.", call. = FALSE)
  }
  if (range[1] > range[2])
  {
    warning("range was ordered.", call. = FALSE)
    range_ordered <- range[order(range)]
    assign("range", range_ordered, pos = parent.frame())
  }
  return(invisible(TRUE))
}

CheckSanitypositivenumeric <- function(mynumeric, varname = "numericvar")
{
  if (length(mynumeric) != 1)
  {
    stop(sprintf("The length of %s must be 1.", varname), call. = FALSE)
  }
  if (is.na(mynumeric))
  {
    stop(sprintf("%s has NA. NA is unacceptable.", varname), call. = FALSE)
  }
  if (!is.numeric(mynumeric))
  {
    stop(sprintf("%s must be numeric.", varname), call. = FALSE)
  }
  if (mynumeric <= 0)
  {
    stop(sprintf("%s must be greater than 0.", varname), call. = FALSE)
  }
  return(invisible(TRUE))  
}

CheckSanitypositive0numeric <- function(mynumeric, varname = "numericvar")
{
  if (length(mynumeric) != 1)
  {
    stop(sprintf("The length of %s must be 1.", varname), call. = FALSE)
  }
  if (is.na(mynumeric))
  {
    stop(sprintf("%s has NA. NA is unacceptable.", varname), call. = FALSE)
  }
  if (!is.numeric(mynumeric))
  {
    stop(sprintf("%s must be numeric.", varname), call. = FALSE)
  }
  if (mynumeric < 0)
  {
    stop(sprintf("%s must be greater than or equal to 0.", varname), call. = FALSE)
  }
  return(invisible(TRUE))  
}

CheckSanitynumeric <- function(mynumeric, varname = "numericvar")
{
  if (length(mynumeric) != 1)
  {
    stop(sprintf("The length of %s must be 1.", varname), call. = FALSE)
  }
  if (is.na(mynumeric))
  {
    stop(sprintf("%s has NA. NA is unacceptable.", varname), call. = FALSE)
  }
  if (!is.numeric(mynumeric))
  {
    stop(sprintf("%s must be numeric.", varname), call. = FALSE)
  }
  return(invisible(TRUE))  
}

CheckSanitynumericvec <- function(numericvec, varname = "numericvec")
{
  if (length(numericvec) < 1)
  {
    stop(sprintf("The length of %s must be greater than or equal to 1.", varname), call. = FALSE)
  }
  if (any(is.na(numericvec)))
  {
    stop(sprintf("%s has NA. NA is unacceptable.", varname), call. = FALSE)
  }
  if (!is.numeric(numericvec))
  {
    stop(sprintf("%s must be numeric.", varname), call. = FALSE)
  }
  return(invisible(TRUE))  
}

CheckSanitylogical <- function(mylogical, varname = "logicalcvar")
{
  if (length(mylogical) != 1)
  {
    stop(sprintf("The length of %s must be 1.", varname), call. = FALSE)
  }
  if (is.na(mylogical))
  {
    stop(sprintf("%s has NA. NA is unacceptable.", varname), call. = FALSE)
  }
  if (!is.logical(mylogical))
  {
    stop(sprintf("%s must be logical.", varname), call. = FALSE)
  }
  return(invisible(TRUE))  
}

CheckSanityimormat <- function(imormat)
{
    if (!is.cimg(imormat)) 
    {
        if (!is.matrix(imormat))
        {
            stop("imormat must be a image of class cimg or a numeric matrix.", call. = FALSE)
        }
        if (any(is.na(imormat)))
        {
            stop("imormat has NA. NA is unacceptable.", call. = FALSE)
        }
        if (!is.numeric(imormat))
        {
            stop("imormat must be numeric", call.=FALSE)
        }
    }
    else 
    {
        if (depth(imormat) != 1) 
        {
            stop("the depth of imormat must be 1.", call. = FALSE)
        }
        if (spectrum(imormat) != 1) 
        {
            stop("imormat must not be a color image.", call. = FALSE)
        }
        if (any(is.na(imormat))) 
        {
            stop("imormat has NA. NA is unacceptable.", call. = FALSE)
        }
        if (!is.numeric(imormat))
        {
            stop("imormat must be numeric.", call. = FALSE)
        }
    }
    return(invisible(TRUE))    
}

CheckSanityimorpx <- function(imorpx)
{
    if (is.cimg(imorpx)) 
    {
        if (depth(imorpx) != 1) 
        {
            stop("the depth of imorpx must be 1.", call. = FALSE)
        }
        if (spectrum(imorpx) != 1) 
        {
            stop("imorpx must be a grayscale image.", call. = FALSE)
        }
        if (any(is.na(imorpx))) 
        {
            stop("imorpx has NA. NA is unacceptable.", call. = FALSE)
        }
        if (!is.numeric(imorpx))
        {
            stop("imorpx has invalid values.", call. = FALSE)
        }
    } else 
    {
        if (!is.pixset(imorpx))
        {
            stop("imorpx must be a image of class cimg or a pixel set.", call. = FALSE)
        }
        if (depth(imorpx) != 1) 
        {
            stop("the depth of imorpx must be 1.", call. = FALSE)
        }
        if (spectrum(imorpx) != 1) 
        {
            stop("imorpx must be a grayscale image.", call. = FALSE)
        }
        if (any(is.na(imorpx)))
        {
            stop("imorpx has NA. NA is unacceptable.", call. = FALSE)
        }
    }
    return(invisible(TRUE))    
}

CheckSanitychar <- function(mychar, varname = "char")
{
  if (length(mychar) < 1)
  {
    stop(sprintf("The length of %s must be greater than 0.", varname), call. = FALSE)
  }
  if (any(is.na(mychar)))
  {
    stop(sprintf("%s has NA. NA is unacceptable.", varname), call. = FALSE)
  }
  if (!is.character(mychar))
  {
    stop(sprintf("%s must be character.", varname), call. = FALSE)
  }
  return(invisible(TRUE))  
}