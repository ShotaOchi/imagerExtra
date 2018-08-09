#include <Rcpp.h>

#define N_PARAMS 2

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

double calc_fuzzy_entropy(Rcpp::NumericVector imhist, Rcpp::NumericVector interval, int idx_a, int idx_c)
{
  int n = imhist.size();
  double a = interval[idx_a];
  double c = interval[idx_c];
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

bool check_dupl(Rcpp::IntegerVector vec)
{
  int n = vec.size();
  bool res = false;
  for (int i = 0; i < n - 1; ++i)
  {
    if (vec[i] == vec[i+1])
    {
      res = true;
      break;
    }
  }
  return res;
}

Rcpp::IntegerVector generate_pos(int n_interval)
{
  Rcpp::IntegerVector res(N_PARAMS);
  if (n_interval < N_PARAMS)
  {
    Rcpp::Rcout << "n_interval is smaller than " << N_PARAMS << "." << std::endl;
    return res;
  }
  Rcpp::NumericVector tmprand = Rcpp::runif(N_PARAMS, 0, n_interval);
  for (int i = 0; i < N_PARAMS; ++i)
  {
    res[i] = (int)(tmprand[i]);
  }
  std::sort(res.begin(), res.end());
  bool flag_dupl = check_dupl(res);
  while (flag_dupl)
  {
    tmprand = Rcpp::runif(N_PARAMS, 0, n_interval);
    for (int i = 0; i < N_PARAMS; ++i)
    {
      res[i] = (int)(tmprand[i]);
    }
    std::sort(res.begin(), res.end());
    flag_dupl = check_dupl(res);
  }
  return res;
}

Rcpp::IntegerMatrix generate_inipos(int n, int n_interval)
{
  Rcpp::IntegerMatrix res(n, N_PARAMS);
  for (int i = 0; i < n; ++i)
  {
    Rcpp::IntegerVector tmp = generate_pos(n_interval);
    for (int j = 0; j < N_PARAMS; ++j)
    {
      res(i,j) = tmp[j];
    }
  }
  return res;
}

Rcpp::NumericMatrix generate_iniv(int n, double vmax)
{
  Rcpp::NumericMatrix res(n, N_PARAMS);
  for (int i = 0; i < n; ++i)
  {
    Rcpp::NumericVector tmp = Rcpp::runif(N_PARAMS, 0, 1);
    for (int j = 0; j < N_PARAMS; ++j)
    {
      res(i,j) = vmax * (tmp[j] + tmp[j] - 1);
    }
  }
  return res;
}

// [[Rcpp::export]]
double fuzzy_threshold(Rcpp::NumericVector imhist, Rcpp::NumericVector interval, int n, int maxiter, double omegamax, double omegamin, double c1, double c2, double mutrate, double vmax, int localsearch)
{
  // sanity ckeck
  if (imhist.size() != interval.size())
  {
    Rcpp::Rcout << "The length of imhist is not same as the length of interval." << std::endl;
    return 0.0;
  }
  if (maxiter < 2)
  {
    Rcpp::Rcout << "maxiter must be greater than or equal to 2." << std::endl;
    return 0.0;
  }
  
  int n_interval = interval.size();
  Rcpp::IntegerMatrix pos = generate_inipos(n, n_interval);
  Rcpp::NumericMatrix v = generate_iniv(n, vmax);
  Rcpp::IntegerVector gbest(N_PARAMS);
  double gbeste = 0;
  double omegacoef = (omegamax - omegamin) / (maxiter - 1);
  Rcpp::IntegerMatrix pbest(n,N_PARAMS); // a, c(from left to right)
  Rcpp::NumericVector pbeste(n); // maximum entropy of each particles
  double vmax_squared = vmax * vmax;
  Rcpp::IntegerMatrix prepos(n,N_PARAMS);
  Rcpp::NumericMatrix prev(n,N_PARAMS);
  double sigma = 0.1 * n_interval;
  
  for (int i = 0; i < n; ++i)
  {
    for (int j = 0; j < N_PARAMS; ++j)
    {
      pbest(i,j) = pos(i,j);
      prepos(i,j) = pos(i,j);
      prev(i,j) = v(i,j);
    }
    pbeste[i] = calc_fuzzy_entropy(imhist, interval, pos(i,0), pos(i,1));
    if (pbeste[i] > gbeste)
    {
      for (int j = 0; j < N_PARAMS; ++j)
      {
        gbest[j] = pbest(i,j);
      }
      gbeste = pbeste[i];
    }   
  }
  for(int k = 1; k < maxiter; ++k)
  {
    double omegak = omegamax - k * omegacoef;
    for (int i = 0; i < n; ++i)
    {
      bool flag_range = false;
      for (int j = 0; j < N_PARAMS; ++j)
      {
        Rcpp::NumericVector temprunif = Rcpp::runif(2);
        v(i,j) = v(i,j) * omegak + c1 * temprunif[0] * (pbest(i,j) - prepos(i,j)) + c2 * temprunif[1] * (gbest[j] - prepos(i,j));
        pos(i,j) = (int)(prepos(i,j) + prev(i,j));
        if (pos(i,j) < 0 || pos(i,j) >= n_interval)
        {
          flag_range = true;
        }
      }
      double vmag = 0.0;
      for (int j = 0; j < N_PARAMS; ++j)
      {
        vmag += v(i,j) * v(i,j);
      }
      if (vmag > vmax_squared)
      {
        double vmag_sqrt = sqrt(vmag);
        for (int j = 0; j < N_PARAMS; ++j)
        {
          v(i,j) *= vmax / vmag_sqrt;
        }
      }
      for (int j = 0; j < N_PARAMS - 1; ++j)
      {
        if (pos(i,j) >= pos(i,j+1))
        {
          flag_range = true;
        }
      }
      if (flag_range)
      {
        Rcpp::IntegerVector tmp_newpos = generate_pos(n_interval);
        for (int j = 0; j < N_PARAMS; ++j)
        {
          pos(i,j) = tmp_newpos[j];
        }
      }
      double tempe = calc_fuzzy_entropy(imhist, interval, pos(i,0), pos(i,1));
      //gaussian mutation
      Rcpp::NumericVector mutran = Rcpp::runif(1);
      if (mutran[0] <= mutrate) 
      {
        Rcpp::NumericVector tempgaus = Rcpp::rnorm(N_PARAMS,0,sigma);
        Rcpp::IntegerVector tmppos(N_PARAMS);
        bool bool_gaus = true;
        for (int j = 0; j < N_PARAMS; ++j)
        {
          tmppos[j] = (int)(pos(i,j) * (1 + tempgaus[j]));
          if (tmppos[j] < 0 || tmppos[j] >= n_interval)
          {
            bool_gaus = false;
          }
        }
        for (int j = 0; j < N_PARAMS - 1; ++j)
        {
          if (tmppos[j] >= tmppos[j+1])
          {
            bool_gaus = false;
          }
        }
        if (bool_gaus)
        {
          double mute = calc_fuzzy_entropy(imhist, interval, tmppos[0], tmppos[1]);
          if (mute > tempe)
          {
            for (int j = 0; j < N_PARAMS; ++j)
            { 
              pos(i,j) = tmppos[j];
            }
            tempe = mute;
          }
        }
      }
      
      if (tempe >= pbeste[i])
      {
        for (int j = 0; j < N_PARAMS; ++j)
        {
          pbest(i,j) = pos(i,j);
        }
        pbeste[i] = tempe;
        if (tempe >= gbeste)
        {
          for (int j = 0; j < N_PARAMS; ++j)
          {
            gbest[j] = pos(i,j);
          }
          gbeste = tempe;
        }
      }
      for (int j = 0; j < N_PARAMS; ++j)
      {
        prepos(i,j) = pos(i,j);
        prev(i,j) = v(i,j);
      }
    }
  }

  // local search
  Rcpp::IntegerVector localmin(N_PARAMS);
  Rcpp::IntegerVector localmax(N_PARAMS);
  for (int j = 0; j < N_PARAMS; ++j)
  {
    localmin[j] = gbest[j] - localsearch  > 0 ? gbest[j] - localsearch : 0;
    localmax[j] = gbest[j] + localsearch < n_interval ? gbest[j] + localsearch : n_interval - 1;
  }
  for (int ia = localmin[0]; ia <= localmax[0]; ++ia)
  {
    for (int ic = localmin[1]; ic <= localmax[1]; ++ic)
    {
      if (ia < ic)
      {
        double tempe = calc_fuzzy_entropy(imhist, interval, ia, ic);
        if (tempe > gbeste)
        {
          gbest[0] = ia;
          gbest[1] = ic;
          gbeste = tempe;
        }
      }
    }
  }  
  
  return (interval[gbest[0]] + interval[gbest[1]]) / 2;
}