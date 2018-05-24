/*
 * Copyright (c) 2011 Catalina Sbert <catalina.sbert@uib.es>
 * All rights reserved
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

/**
 * @brief inverse cumulative distribution function
 *
 * Given a value Fu, computes the inverse of the cumulative histogram
 * by searching the appropriate value in the array of sorted data
 *
 * @param Fu  value (> 0)
 * @param F  sorted data
 * @return x the inverse value of Fu
 */
double inverse_cumulative_function(double Fu, Rcpp::NumericVector F) 
{
    double x;
    int    pos;

	/* sanity check*/
    if (Fu < 0) 
	{
        Rcpp::Rcout << "Error: Fu is lower than 0" << std::endl;
        return 0;
    }
	
    pos = (int) ceil((double) Fu);
    pos = pos - 1; /*array indexes start at 0*/

    x = F[pos];

    return x;
}

/**
* @brief Affine Transformation
*
* This function is the affine transformation of the interval [x0,x1] into
* [y0,y1]
*
* @param data input array
* @param x0, x1 initial interval
* @param y0, y1 transformed interval

@return data_out output array
*/
void affine_transformation(Rcpp::NumericVector data, int n, double* ptr_data_out, double x0, double x1, double y0, double y1, double max_range, double min_range) 
{
    double slope;

    slope = (y1 - y0) / (x1 - x0);

    for (int i = 0; i < n; ++i) 
	{
        if (x0 <= data[i] && data[i] <= x1) 
		{
            ptr_data_out[i] = y0 + slope * (data[i] - x0);
            if (ptr_data_out[i] > max_range) 
			{
                 ptr_data_out[i] = max_range;
			}
            if (ptr_data_out[i] < min_range) 
			{
                ptr_data_out[i] = min_range;
			}
        }
    }
}

/**
* @brief Main block of the algorithm
*
* Given a regular partition of [0,255] with N {y_k} control points
* (N=number of intervals of the partition - 1), compute a new partition
* as the inverse of the cumulative histogram of the initial data  {x_k}.
*
* Then apply an affine transform which transforms [x_{k-1}, x_k] into
* [y_{k-1}, y_k]. The slope of this application is s_k.
* We impose a constraint on the  slope for this affine transform
*
* @f$ m_k=\cases{max(s_k, smin) & if $s_k <1$ \cr min(s_k, smax) & if $s_k >1$\cr} \f$
*
* @param data initial array
* @param F sorted initial array
* @param dim size of the array
* @param N number of control points (number of intervals of the partition - 1)
* @param smax maximum slope allowed
* @param smin minimum slope allowed
* @param min minimum of initial array
* @param max maximum of initial array
* @param max_range maximum of the range of the value
* @param min_range minimum of the range of the value
* transformation
*
*/
// [[Rcpp::export]]
Rcpp::NumericVector piecewise_transformation(Rcpp::NumericVector data, Rcpp::NumericVector F, int N, double smax, double smin, double max, double min, double max_range, double min_range) 
{
    double x0, x1, y0, y1;
    double Fu;
    int    k;
    double slope;
	
	int n = data.size();
    Rcpp::NumericVector data_out(n);
	data_out.fill(0.0);
	double* ptr_data_out = data_out.begin();

    x0 = min;
    y0 = min_range;

    for (k = 1; k <= N; k++) 
	{
        Fu = (double) (k * n) / (double) (N + 1);
        y1 = (max_range * (double) k) / (double) (N + 1);
        x1 = inverse_cumulative_function(Fu, F);
        if (x1 > x0) 
		{
            slope = (y1 - y0) / (x1 - x0);

            if (slope > smax) 
			{
                y1 = smax * (x1 - x0) + y0;
			}
            if (slope < smin) 
			{
                y1 = smin * (x1 - x0) + y0;
			}
            affine_transformation(data, n, ptr_data_out, x0, x1, y0, y1, max_range, min_range);
            x0 = x1;
            y0 = y1;
        }
    }
    if (x0 < max) 
	{
        y1 = max_range;
        x1 = max;
        slope = (y1 - y0) / (x1 - x0);
        if (slope > smax) 
		{
            y1 = smax * (x1 - x0) + y0;
		}
        if (slope < smin) 
		{
            y1 = smin * (x1 - x0) + y0;
		}
        affine_transformation(data, n, ptr_data_out, x0, x1, y0, y1, max_range, min_range);
    }
	
    return data_out;
}
