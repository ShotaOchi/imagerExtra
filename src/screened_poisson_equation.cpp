/*
 *
 * Copyright 2012 IPOL Image Processing On Line http://www.ipol.im/
 *
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
 *
 *
 * @file screened_lib.c
 * @brief laplacian, DFT and Poisson routines
 *
 * @author Catalina Sbert <catalina.sbert@uib.es>
 */

#include <Rcpp.h>

/* M_PI is a POSIX definition */
#ifndef M_PI2
/** macro definition for Pi² */
#define M_PI2 9.86960440109
#endif                          /* !M_PI */

/**
 * @brief perform a screned  Poisson PDE in the Fourier DCT space
 *
 * @f$ (PI² i²/nx²+ PI²j²/ny²+ lambda)u(i, j) = 
 *    =(PI² i²/nx²+ PI²j²/ny²) g(i,j) @f$
 *
 * @param data  input array dct of the input image of size nx x ny
 * @param nx data array size
 * @param ny data array size
 * @param L the constant of the screened equation
 *
 * @return the data array, update
 */
// [[Rcpp::export]]
Rcpp::NumericMatrix screened_poisson_dct(Rcpp::NumericMatrix data, double L)
{
    int nx = data.nrow();
    int ny = data.ncol();
    Rcpp::NumericMatrix data_out(Rcpp::Dimension(nx, ny));
    double normx, normy, coeff, coeff1;
    normx = 4.0 * M_PI2 / (double)(nx * nx);
    normy = 4.0 * M_PI2 / (double)(ny * ny);

    if (L > 0.)
    {
        for (int i = 0; i < nx; ++i)
        {
            for (int j = 0; j < ny; ++j)
            {
                if (i == 0 && j == 0) 
                {
                    data_out(0, 0) = 0.;
                } else
                {
                    coeff = normx * i * i + normy * j * j;
                    coeff1 = coeff / (coeff + L);
                    data_out(i, j) = data(i, j) * coeff1;
                }
            }
        }
    }
    return data_out;
}   