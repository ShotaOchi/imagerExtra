#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector make_histogram_fuzzy(Rcpp::NumericVector ordered, Rcpp::NumericVector interval)
{
  int n = ordered.size();
  int m = interval.size();
  Rcpp::NumericVector res(m);
  int count = 0;
  for (int i = 0; i < n; ++i)
  {
    if (ordered[i] <= interval[count])
    {
      ++res[count];
    } else 
    {
      while (ordered[i] > interval[count])
      {
        ++count; 
      }
      if (count >= m)
      {
        break;
      }
      ++res[count]; 
    }
  }
  return res;
}

double calc_fuzzy_entropy(Rcpp::NumericVector imhist, Rcpp::NumericVector interval, double a, double c)
{
  int n = imhist.size();
  double b = (a + c) / 2;
  double res = 0.0;
  for (int i = 0; i < n; ++i)
  {
    double mu = 0;
    if (interval[i] > a)
    {
      if (interval[i] < b)
      {
        mu = 2 * ((interval[i] - a) / (c - a)) * ((interval[i] - a) / (c - a));
      } else if (interval[i] < c)
      {
        mu = 1 - 2 * ((c - interval[i]) / (c - a)) * ((c - interval[i]) / (c - a));
      } else 
      {
        mu = 1;
      }
    }
    double shannonf = 0.0;
    if (mu != 0.0 && mu != 1.0)
    {
      shannonf = -mu * std::log(mu) - (1 - mu) * std::log(1 - mu);
    }
    res += shannonf * imhist[i];
  }
  return res;
}

// [[Rcpp::export]]
double fuzzy_threshold(Rcpp::NumericVector imhist, Rcpp::NumericVector interval, Rcpp::NumericMatrix pos, Rcpp::NumericMatrix v, int n, int maxiter, double omegamax, double omegamin, double c1, double c2, double mutrate, double vmax, double localsearch, double maxval, double minval)
{
  // sanity ckeck
  if (pos.ncol() != 2 || v.ncol() != 2 || pos.nrow() != n || v.nrow() != n)
  {
    Rcpp::Rcout << "dimension of pos or v is not appropriate." << std::endl;
    return 0.0;
  }
  if (imhist.size() != interval.size())
  {
    Rcpp::Rcout << "The length of imhist is not same as the length of interval" << std::endl;
    return 0.0;
  }

  double gbesta = 0;
  double gbestc = 0;
  double gbeste = 0;
  double omegacoef = (omegamax - omegamin) / (maxiter - 1);
  Rcpp::NumericMatrix pbest(n,3); // a, c, entropy (from left to right)
  double vmax_squared = vmax * vmax;
  Rcpp::NumericMatrix prepos(n,2);
  Rcpp::NumericMatrix prev(n,2);
  double sigma = 0.1 * (maxval - minval);
  
  for(int j = 0; j < n; ++j)
  {
    pbest(j,0) = pos(j,0);
    pbest(j,1) = pos(j,1);
    prepos(j,0) = pos(j,0);
    prepos(j,1) = pos(j,1);
    prev(j,0) = v(j,0);
    prev(j,1) = v(j,1);
    pbest(j,2) = calc_fuzzy_entropy(imhist, interval, pos(j,0), pos(j,1));
    if (pbest(j,2) > gbeste)
    {
      gbesta = pbest(j,0);
      gbestc = pbest(j,1);
      gbeste = pbest(j,2);
    }   
  }
  for(int k = 1; k < maxiter; ++k)
  {
    double omegak = omegamax - k * omegacoef;
    for (int j = 0; j < n; ++j)
    {
      Rcpp::NumericVector temprunif = Rcpp::runif(4); 
      v(j,0) = v(j,0) * omegak + c1 * temprunif[0] * (pbest(j,0) - prepos(j,0)) + c2 * temprunif[1] * (gbesta - prepos(j,0));
      v(j,1) = v(j,1) * omegak + c1 * temprunif[2] * (pbest(j,1) - prepos(j,1)) + c2 * temprunif[3] * (gbestc - prepos(j,1));
      double vmag = v(j,0) * v(j,0) + v(j,1) * v(j,1); 
      if (vmag > vmax_squared)
      {
        double vmag_sqrt = sqrt(vmag);
        v(j,0) *= vmax / vmag_sqrt;
        v(j,1) *= vmax / vmag_sqrt;
      }
      pos(j,0) = prepos(j,0) + prev(j,0);
      pos(j,1) = prepos(j,1) + prev(j,1);
      if (pos(j,0) < minval || pos(j,0) > maxval || pos(j,1) < minval || pos(j,1) > maxval || pos(j,0) >= pos(j,1))
      {
        Rcpp::NumericVector tempunif1 = Rcpp::runif(1, 0, 0.999);
        Rcpp::NumericVector tempunif2 = Rcpp::runif(1, tempunif1[0], 1);
        pos(j,0) = minval + tempunif1[0] * (maxval - minval);
        pos(j,1) = minval + tempunif2[0] * (maxval - minval);
      }
      

      double tempe = calc_fuzzy_entropy(imhist, interval, pos(j,0), pos(j,1));
      //gaussian mutation
      Rcpp::NumericVector mutran = Rcpp::runif(1);
      if (mutran[0] <= mutrate) 
      {
        Rcpp::NumericVector tempgaus = Rcpp::rnorm(2,0,sigma);
        double temppos1 = pos(j,0) * (1 + tempgaus[0]);
        double temppos2 = pos(j,1) * (1 + tempgaus[1]);
        if (temppos1 >= minval && temppos1 <= maxval && temppos2 >= minval && temppos2 <= maxval && temppos1 < temppos2)
        {
          double mute = calc_fuzzy_entropy(imhist, interval, temppos1, temppos2);
          if (mute > tempe)
          {
            pos(j,0) = temppos1;
            pos(j,1) = temppos2;
            tempe = mute;
          }
        }
      }
      
      if (tempe >= pbest(j,2))
      {
        pbest(j,0) = pos(j,0);
        pbest(j,1) = pos(j,1);
        pbest(j,2) = tempe;
        if (tempe >= gbeste)
        {
          gbesta = pos(j,0);
          gbestc = pos(j,1);
          gbeste = tempe;
        }
      }
      prepos(j,0) = pos(j,0);
      prepos(j,1) = pos(j,1);
      prev(j,0) = v(j,0);
      prev(j,1) = v(j,1);
    }
    
  }

  // local search
  double localamin = gbesta - localsearch > minval ? gbesta - localsearch : minval;
  double localamax = gbesta + localsearch < maxval ? gbesta + localsearch : maxval;
  double localcmin = gbestc - localsearch > minval ? gbestc - localsearch : minval;
  double localcmax = gbestc + localsearch < maxval ? gbestc + localsearch : maxval;
  double stepsize = localsearch / 50;
  for (double ia = localamin; ia <= localamax; ia += stepsize)
  {
    for (double ic = localcmin; ic <= localcmax; ic += stepsize)
    {
      if (ia < ic)
      {
        double tempe = calc_fuzzy_entropy(imhist, interval, ia, ic);
        if (tempe > gbeste)
        {
          gbesta = ia;
          gbestc = ic;
          gbeste = tempe;
        }
      }
    }
  }  
  
  return (gbesta + gbestc) / 2;
}