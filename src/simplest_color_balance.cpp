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

