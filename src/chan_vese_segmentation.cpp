/**
 * @file chanvese.c
 * @brief Chan-Vese active contours without edges image segmentation
 * @author Pascal Getreuer <getreuer@gmail.com>
 *
 * This file implements Chan-Vese active contours without edges two-phase
 * image segmentation.
 *
 * 
 * Copyright (c) 2007-2012, Pascal Getreuer
 * All rights reserved.
 * 
 * This program is free software: you can use, modify and/or 
 * redistribute it under the terms of the simplified BSD License. You 
 * should have received a copy of this license along this program. If 
 * not, see <http://www.opensource.org/licenses/bsd-license.html>.
 */

#include <Rcpp.h>
#define DIVIDE_EPS       ((double)1e-16)

/** @brief Default initialization for Phi */
// [[Rcpp::export]]
Rcpp::NumericMatrix ChanVeseInitPhi(int Width, int Height)
{
  Rcpp::NumericMatrix res(Width, Height);
  for(int i = 0; i < Width; ++i)
  {
    for(int j = 0; j < Height; ++j)
    {
      res(i,j) = (double)(sin(i*M_PI/5.0)*sin(j*M_PI/5.0));
    }
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix ChanVeseInitPhi_Rect(int Width, int Height, Rcpp::IntegerVector rect)
{
  Rcpp::NumericMatrix res(Width, Height);
  int len_rect = rect.length();
  if (len_rect != 4)
  {
    Rcpp::Rcout << "rect is not appropriate." << std::endl;
    return res;
  }
  int x0 = rect[0];
  int y0 = rect[1];
  int x1 = rect[2];
  int y1 = rect[3];
  if (x0 > x1)
  {
    int tmp_x = x0;
    x0 = x1;
    x1 = tmp_x;
  }
  if (y0 > y1)
  {
    int tmp_y = y0;
    y0 = y1;
    y1 = tmp_y;
  }
  
  for(int i = 0; i < Width; ++i)
  {
    for(int j = 0; j < Height; ++j)
    {
      if (i >= x0 && i <= x1 && j >= y0 && j <= y1)
      {
        res(i,j) = 1;
      } else
      {
        res(i,j) = -1;
      }
    }
  }
  return res;
}

/** @brief Compute averages inside and outside of the segmentation contour */
void RegionAverages_ChanVese(double *c1, double *c2, const Rcpp::NumericMatrix& Phi, const Rcpp::NumericMatrix& f, const int Width, const int Height)
{
    const long NumPixels = ((long)Width) * ((long)Height);
    double Sum1 = 0, Sum2 = 0;
    long Count1 = 0, Count2 = 0;
    for (long n = 0; n < NumPixels; ++n)
    {
      if (Phi[n] >= 0)
      {
        ++Count1;
        Sum1 += f[n];
      }
      else
      {
        ++Count2;
        Sum2 += f[n];
      }
    }
    *c1 = (Count1) ? (Sum1/Count1) : 0;
    *c2 = (Count2) ? (Sum2/Count2) : 0;
}

/**
 * @brief Chan-Vese two-phase image segmentation
 * @param Phi pointer to array to hold the resulting segmentation
 * @param f the input image
 * @param Width, Height, NumChannels the size of f
 * @param Tol convergence tolerance
 * @param MaxIter maximum number of iterations
 * @param Mu length penalty
 * @param Nu area penalty (positive penalizes area inside the curve)
 * @param Lambda1 fit penalty inside the curve
 * @param Lambda2 fit penalty outside the curve
 * @param dt timestep
 * @param PlotFun function for outputting intermediate results
 *
 * This function performs Chan-Vese active contours two-phase image 
 * segmentation by minimizing the functional 
 * \f[ \begin{aligned}\operatorname*{arg\,min}_{c_1,c_2,C}\;& \mu 
 * \operatorname{Length}(C) + \nu\operatorname{Area}(\mathit{inside}(C)) \\
 * &+ \lambda_1 \int_{\mathit{inside}(C)}|f(x)-c_1|^2 \, dx + \lambda_2
 * \int_{\mathit{outside}(C)} |f(x) - c_2|^2 \, dx, \end{aligned} \f]
 * where the minimization is over all set boundaries C and scalars c1 and c2.
 * The boundary C is implicitly represented by level set function Phi.
 * 
 * The input f can be a grayscale image or an image with any number of
 * channels, i.e., three channels for a color image, or possibly many more in a
 * hyperspectral image.  If f is a multichannel image, the segmentation is done
 *  using the Chan, Sandberg, Vese vector extension of the Chan-Vese model,
 * \f[ \begin{aligned}\operatorname*{arg\,min}_{c_1,c_2,C}\;& \mu 
 * \operatorname{Length}(C)+\nu\operatorname{Area}(\mathit{inside}(C)) \\ &+ 
 * \lambda_1 \int_{\mathit{inside}(C)}\|f(x)-c_1\|^2 \,dx+\lambda_2\int_{
 * \mathit{outside}(C)}\|f(x)-c_2\|^2\,dx,\end{aligned} \f]
 * where \f$ \|\cdot\| \f$ denotes the Euclidean norm. 
 *  
 * The data for f should be stored as a contiguous block of data of 
 * Width*Height*NumChannels elements, where the elements are ordered so that
 *   f[x + Width*(y + Height*k)] = kth component of the pixel at (x,y)
 *
 * The array Phi is a contiguous array of size Width by Height with the same
 * order as f.  Phi is a level set function of the segmentation, meaning the
 * segmentation is indicated by its sign:
 *    Phi[x + Width*y] >= 0 means (x,y) is inside the segmentation curve,
 *    Phi[x + Width*y] <  0 means (x,y) is outside.
 * Before calling this routine, Phi should be initialized either by calling 
 * InitPhi or by setting it to a level set function of an initial guess of the
 * segmentation.  After this routine, the final segmentation is obtained from
 * the sign of Phi.
 *  
 * The routine runs at most MaxIter number of iterations and stops when the
 * change between successive iterations is less than Tol.  Set Tol=0 to force
 * the routine to run exactly MaxIter iterations.
 */
 // [[Rcpp::export]]
Rcpp::List ChanVese(Rcpp::NumericMatrix im, double Mu, double Nu, double Lambda1, double Lambda2, double tol, int maxiter, double dt, Rcpp::NumericMatrix phi)
{
  int nrow = im.nrow();
  int ncol = im.ncol();
  double NumPixels = nrow * ncol;
  double PhiDiff;
  double c1scalar = 0, c2scalar = 0;
  double *c1 = &c1scalar, *c2 = &c2scalar;
  double PhiLast, Delta, PhiX, PhiY, IDivU, IDivD, IDivL, IDivR;
  double Dist1, Dist2;
  int iu, id, il, ir;
  double PhiDiffNorm = (tol > 0) ? tol*1000 : 1000;
  
  int last_iter = 0;
    
  RegionAverages_ChanVese(c1, c2, phi, im, nrow, ncol);
    
  for (int Iter = 1; Iter <= maxiter; ++Iter)
  {
    PhiDiffNorm = 0;
    for (int j = 0; j < ncol; ++j)
    {
      iu = (j == 0) ? 0 : -1;
      id = (j == ncol - 1) ? 0 : 1;
      
      for (int i = 0; i < nrow; ++i)
      {
        il = (i == 0) ? 0 : -1;
        ir = (i == nrow - 1) ? 0 : 1;
        
        Delta = dt/(M_PI*(1 + phi(i,j) * phi(i,j)));
        PhiX = phi(i+ir,j) - phi(i,j);
        PhiY = (phi(i,j+id) - phi(i,j+iu))/2;
        IDivR = (double)(1/sqrt(DIVIDE_EPS + PhiX*PhiX + PhiY*PhiY));
        PhiX = phi(i,j) - phi(i+il,j);
        IDivL = (double)(1/sqrt(DIVIDE_EPS + PhiX*PhiX + PhiY*PhiY));
        PhiX = (phi(i+ir,j) - phi(i+il,j))/2;
        PhiY =  phi(i,j+id) - phi(i,j);
        IDivD = (double)(1/sqrt(DIVIDE_EPS + PhiX*PhiX + PhiY*PhiY));
        PhiY = phi(i,j) - phi(i,j+iu);
        IDivU = (double)(1/sqrt(DIVIDE_EPS + PhiX*PhiX + PhiY*PhiY));
        
        Dist1 = im(i,j) - *c1;
        Dist2 = im(i,j) - *c2;
        Dist1 *= Dist1;
        Dist2 *= Dist2;
        
        /* Semi-implicit update of phi at the current point */
        PhiLast = phi(i,j);
        phi(i,j) = (phi(i,j) + Delta*(
          Mu*(phi(i+ir,j)*IDivR + phi(i+il,j)*IDivL
          + phi(i,j+id)*IDivD + phi(i,j+iu)*IDivU)
          - Nu - Lambda1*Dist1 + Lambda2*Dist2) ) /
          (1 + Delta*Mu*(IDivR + IDivL + IDivD + IDivU));
        PhiDiff = (phi(i,j) - PhiLast);
        PhiDiffNorm += PhiDiff * PhiDiff;
      }
    }
    PhiDiffNorm = sqrt(PhiDiffNorm/NumPixels);
    RegionAverages_ChanVese(c1, c2, phi, im, nrow, ncol);
    
    if (Iter >= 2 && PhiDiffNorm <= tol)
    {
      last_iter = Iter;
      break;
    }
  }
  
  if (last_iter == 0)
  {
    last_iter = maxiter;
  }
  return Rcpp::List::create(Rcpp::Named("num_iter") = last_iter, Rcpp::Named("result") = phi);    
}