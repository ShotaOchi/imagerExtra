#' Chan-Vese segmentation
#'
#' iterative image segmentation with Chan-Vese model
#' @param im a grayscale image of class cimg
#' @param mu length penalty
#' @param nu area penalty
#' @param lambda1 fit weight inside the cuve
#' @param lambda2 fit weight outside the curve
#' @param tol convergence tolerance
#' @param maxiter maximum number of iterations
#' @param dt time step
#' @param initial "interactive" or a grayscale image of class cimg. you can define initial condition as a rectangle shape interactively if initial is "interactive". If initial is a grayscale image of class cimg, pixels whose values are negative will be treated as outside of contour. pixels whose values are non-negative will be treated as inside of contour. checker board condition will be used if initial is not specified. 
#' @param returnstep a numeric vector that determines which result will be returned. 0 means initial condition, and 1 means the result after 1 iteration. final result will be returned if returnstep is not specified.
#' @return a pixel set or a list of lists of numeric and pixel set
#' @references Pascal Getreuer (2012). Chan-Vese Segmentation. Image Processing On Line 2, 214-224.
#' @author Shota Ochi
#' @export
#' @examples
#' layout(matrix(1:2, 1, 2))
#' g <- grayscale(dogs)
#' plot(g, main = "Original")
#' SegmentCV(g, lambda2 = 15) %>% plot(main = "Binarized")
SegmentCV <- function(im, mu = 0.25, nu = 0.0, lambda1 = 1.0, lambda2 = 1.0, tol = 0.0001, maxiter = 500, dt = 0.5, initial, returnstep)
{
  CheckSanityim(im)
  CheckSanitynumeric(mu, "mu")
  CheckSanitynumeric(nu, "nu")
  CheckSanitynumeric(lambda1, "lambda1")
  CheckSanitynumeric(lambda2, "lambda2")
  CheckSanitypositive0numeric(tol, "tol")
  CheckSanitypositivenumeric(maxiter, "maxiter")
  CheckSanitypositivenumeric(dt, "dt")
  maxiter <- as.integer(maxiter)
  
  dim_im <- dim(im)
  if (missing(initial))
  {
    initial <- ChanVeseInitPhi(dim_im[1], dim_im[2]) %>% as.cimg()
  } else if (!is.null(initial))
  {
    if (is.character(initial))
    {
      #Checksanitychar(initial, "initial")
      pos_rect <- grabRect(im)
      if (pos_rect[1] == pos_rect[3] && pos_rect[2] == pos_rect[4])
      {
        stop("Specifying initial condition failed.", call. = FALSE)
      }
      initial <- ChanVeseInitPhi_Rect(dim_im[1], dim_im[2], pos_rect) %>% as.cimg()
    } else if (is.cimg(initial))
    {
      dim_ini <- dim(initial)
      if (any(dim_ini != dim_im))
      {
        stop("The dimension of initial is not same as dimension of im.", call. = FALSE)
      }
    } else
    {
      stop('initial must be "interactive" or a grayscale image of class cimg', call. = FALSE)
    }
  }
  
  if (missing(returnstep))
  {
    res <- ChanVese(as.matrix(im), mu, nu, lambda1, lambda2, tol, maxiter, dt, as.matrix(initial))
    if (res[[1]] == maxiter)
    {
      message("The computation stopped because the number of iteration reached maxiter.")
    }
    return(as.cimg(res[[2]]) >= 0)
  } else
  {
    CheckSanitynumericvec(returnstep, "returnstep")
    returnstep <- as.integer(returnstep)
    tmpidx_returnstep <- returnstep >= 0 && returnstep <= maxiter
    returnstep <- returnstep[tmpidx_returnstep]
    if (length(returnstep) < 1)
    {
      stop("The elements of returnstep must be in the range [0,maxiter].", call. = FALSE)
    }
    returnstep <- unique(returnstep)
    returnstep <- returnstep[order(returnstep)]
    returnstep2 <- c(0, returnstep)
    res <- list()
    if (returnstep[1] == 0)
    {
      result_first <- initial >= 0
      tmp0 <- list(num_iter = 0, result = result_first)
      res <- c(res, list(tmp0))
      returnstep <- returnstep[2:length(returnstep)]
    }
    pre_phi <- as.matrix(initial)
    for (i in seq(length(returnstep)))
    {
      tmp_maxiter <- returnstep[i] - returnstep2[i]
      tmp_res <- ChanVese(as.matrix(im), mu, nu, lambda1, lambda2, tol, tmp_maxiter, dt, pre_phi)
      pre_phi <- tmp_res[[2]]
      tmp_res[[2]] <- as.cimg(tmp_res[[2]]) >= 0
      tmp_res[[1]] <- tmp_res[[1]] + returnstep2[i]
      res <- c(res, list(tmp_res))
      if (tmp_res[[1]] != returnstep[i])
      {
        message("The computation stopped in the middle because stop criterion was satisified")
        break
      }
    }
    return(res)
  }
  return(NULL)
}