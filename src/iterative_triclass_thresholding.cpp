/*
 * Copyright (c) 2018, Shota Ochi <shotaochi1990@gmail.com>
 * All rights reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector make_prob_otsu(Rcpp::NumericVector ordered, Rcpp::NumericVector bins, int intervalnumber, int width, int height)
{
  Rcpp::NumericVector out(intervalnumber);
  int n = ordered.size();
  int m = bins.size();
  int count = 0;
  for (int i = 0; i < n; ++i)
  {
    if (ordered[i] <= bins[count])
    {
      out[count] += 1;
    } else {
      while (ordered[i] > bins[count])
      {
        ++count;    
      }
      if (count >= intervalnumber || count >= m) 
      {
        break;
      }
      out[count] += 1;
    }
  }
  double size = (double)width * height;
  for (int i = 0; i < intervalnumber; ++i) 
  {
    out[i] /= size;
  }
  return out;
}

double calc_ICV_ostu(double omegak, double myuk, double myut)
{
  if (omegak != 0 && omegak != 1) 
  {
    return (myut * omegak - myuk) * (myut * omegak - myuk) / (omegak * (1 - omegak));
  } else {
    return -1;
  }
}
  
// [[Rcpp::export]]
double get_th_otsu(Rcpp::NumericVector prob_otsu, Rcpp::NumericVector bins)
{
  int n = prob_otsu.size();
  int m = bins.size();
  if (n < 2)
  {
    Rcpp::Rcout << "lengths of prob_otsu must be greater than 1." << std::endl;
    return 0;
  }
  if (n != m)
  {
    Rcpp::Rcout << "lengths of prob_otsu and bins are not same." << std::endl;
    return 0;
  }
    
  double myut = 0.0;
  for (int i = 0; i < n; ++i) 
  {
    myut += prob_otsu[i] * bins[i];
  }

  double omegak = prob_otsu[0];
  double myuk = prob_otsu[0] * bins[0];
  double ICV = calc_ICV_ostu(omegak, myuk, myut);
  double maxICV = ICV;
  double threshold = bins[0];
  for(int i = 1; i < n; ++i)
  {
    omegak += prob_otsu[i];
    myuk += prob_otsu[i] * bins[i];
    ICV = calc_ICV_ostu(omegak, myuk, myut);
    if (ICV > maxICV) 
    {
      maxICV = ICV;
      threshold = bins[i];
    }
  }
  return threshold;
}