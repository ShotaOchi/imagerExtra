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

 //$ The code below was written by modifying piecewise_transformation function in piecewise_transformation.cpp file.
 //$ That's why the copy right holder of the code below is Catalina Sbert.
 
#include <Rcpp.h>


/**
* @brief Main block of Simplest Color Balance
*
* @param data initial array
* @param max_im maximum of the saturated image
* @param min_im minimum of the saturated image
* @param max_range maximum of the range of the pixel values
* @param min_range minimum of the range of the pixel values
**/
// [[Rcpp::export]]
Rcpp::NumericVector saturateim(Rcpp::NumericVector data, double max_im, double min_im, double max_range, double min_range)
{
    int n = data.size();
    Rcpp::NumericVector data_out(n);
    data_out.fill(0.0);
    double* ptr_data_out = data_out.begin();
    double slope = (max_range - min_range) / (max_im - min_im);

    for (int i = 0; i < n; ++i) 
    {
        if (data[i] > max_im)
        {
            ptr_data_out[i] = max_range;
            continue;
        }
        if (data[i] < min_im)
        {
            ptr_data_out[i] = min_range;
            continue;
        }
        ptr_data_out[i] = slope * (data[i] - min_im) + min_range;
    }
    return data_out;
}

