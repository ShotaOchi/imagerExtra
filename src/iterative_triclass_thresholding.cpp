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