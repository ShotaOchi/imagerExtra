#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector make_density_multilevel(Rcpp::NumericVector ordered, Rcpp::NumericVector interval)
{
  int n = ordered.size();
  int m = interval.size();
  if (n == 0) 
  {
    Rcpp::Rcout << "Error: The length of ordered is 0." << std::endl;
    return 0.0;
  }
  if (m == 0) 
  {
    Rcpp::Rcout << "Error: The length of interval is 0." << std::endl;
    return 0.0;
  }
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
  for (int i = 0; i < m; ++i)
  {
    res[i] /= n;
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::NumericVector make_integral_density_multilevel(Rcpp::NumericVector density)
{
  int n = density.size();
  if (n == 0) 
  {
    Rcpp::Rcout << "Error: The length of ordered is 0." << std::endl;
    return 0.0;
  }
  Rcpp::NumericVector res(n);
  double temp = 0.0;
  for (int i = 0; i < n; ++i)
  {
    temp += density[i];
    res[i] = temp;
  }
  return res;
}

double calculate_entropy_multilevel(Rcpp::NumericVector density, Rcpp::NumericVector integral_density, Rcpp::IntegerVector thresholds)
{
  int n = density.size();
  int k = thresholds.size();
  double res = 0.0;
  
  double omega0 = integral_density[thresholds[0]];
  double h0 = 0.0;
  if (omega0 != 0.0)
  {
    for (int j = 0; j <= thresholds[0]; ++j)
    {
      if (density[j] != 0.0)
      {
        h0 += density[j] * std::log(density[j] / omega0) / omega0;
      }
    }
  }
  res -= h0;
  for (int i = 1; i < k; ++i)
  {
    double omegak = integral_density[thresholds[i]] - integral_density[thresholds[i-1]];
    double hk = 0.0;
    for (int j = thresholds[i-1] + 1; j <= thresholds[i]; ++j)
    {
      if (density[j] != 0.0)
      {
        hk += density[j] * std::log(density[j] / omegak) / omegak;
      }
    }
    res -= hk;
  }
  double omegan = integral_density[n-1] - integral_density[thresholds[k-1]];
  double hn = 0.0;
  if (omegan != 0.0)
  {
    for (int j = thresholds[k-1]; j < n; ++j)
    {
      if (density[j] != 0.0)
      {
        hn += density[j] * std::log(density[j] / omegan) / omegan;
      }
    }
  }
  res -= hn;
  return res;
}

Rcpp::IntegerVector generate_inipos_multilevel(int n_thres, int maxnum_interval)
{
  Rcpp::IntegerVector res(n_thres);
  Rcpp::NumericVector tmp = Rcpp::runif(n_thres, 0, maxnum_interval);
  std::sort(tmp.begin(), tmp.end());
  for (int i = 0; i < n_thres; ++i)
  {
    tmp[i] = (int)tmp[i];
  }
  bool tmpbool = false;
  for (int i = 0; i < n_thres - 1; ++i)
  {
    if (tmp[i] == tmp[i+1])
    {
      tmpbool = true;
      break;
    }
  }
  while (tmpbool)
  {
    tmp = Rcpp::runif(n_thres, 0, maxnum_interval);
    std::sort(tmp.begin(), tmp.end());
    for (int j = 0; j < n_thres; ++j)
    {
      tmp[j] = (int)tmp[j];
    }
    tmpbool = false;
    for (int i = 0; i < n_thres - 1; ++i)
    {
      if (tmp[i] == tmp[i+1])
      {
        tmpbool = true;
        break;
      }
    }
  }
  for (int i = 0; i < n_thres; ++i)
  {
    res[i] = (int)tmp[i];
  }
  return res;
}

int generate_randint_multilevel(int n_ex, int maxnum)
{
  if (maxnum <= 1)
  {
    Rcpp::Rcout << "maxnum is smaller than 2 in generate_randint_multilevel." << std::endl;
    return 0;
  }
  Rcpp::NumericVector tmp = Rcpp::runif(1, 0, maxnum);
  int res = (int)tmp[0];
  while (res == n_ex)
  {
    tmp = Rcpp::runif(1, 0, maxnum);
  res = (int)tmp[0];
  }  
  return res;
}

bool check_dupl_multilevel(Rcpp::IntegerVector vec)
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

bool compare_pairsecond_multilevel(std::pair<int, double> x, std::pair<int, double> y)
{
  return x.second > y.second;
}

Rcpp::IntegerVector generate_newpos_multilevel(int j, int sn, int n_thres, int n, Rcpp::IntegerMatrix prepos)
{
  int tmpidx = generate_randint_multilevel(j, sn);
  Rcpp::NumericVector tmprand = Rcpp::runif(n_thres, -1, 1);
  Rcpp::IntegerVector newpos(n_thres);
  for (int k = 0; k < n_thres; ++k)
  {
    int tmppos = (int)(prepos(j,k) + tmprand[k] * (prepos(j,k) - prepos(tmpidx,k)));
    if (tmppos < 0)
  {
    tmppos = 0;
  } else if (tmppos >= n)
  {
    tmppos = n - 1;
  }
  newpos[k] = tmppos;
  }
  std::sort(newpos.begin(), newpos.end());
  bool flag_dupl = check_dupl_multilevel(newpos);
  while (flag_dupl)
  {
    tmpidx = generate_randint_multilevel(j, sn);
    tmprand = Rcpp::runif(n_thres, -1, 1);
    for (int k = 0; k < n_thres; ++k)
    {
      int tmppos = (int)(prepos(j,k) + tmprand[k] * (prepos(j,k) - prepos(tmpidx,k)));
      if (tmppos < 0)
      {
        tmppos = 0;
      } else if (tmppos >= n)
      {
        tmppos = n - 1;
      }
      newpos[k] = tmppos;
    }
    std::sort(newpos.begin(), newpos.end());
    flag_dupl = check_dupl_multilevel(newpos);
  } 
  return newpos;
}

// [[Rcpp::export]]
Rcpp::IntegerVector get_threshold_multilevel(Rcpp::NumericVector im_density, Rcpp::NumericVector im_integral_density, int n_thres, int sn, int mcn, int limit)
{
  int n = im_density.size();
  if (n != im_integral_density.size())
  {
    Rcpp::Rcout << "The length of im_density is not same as the length of im_integral_density." << std::endl;
  }
  Rcpp::IntegerVector gbest(n_thres);
  Rcpp::IntegerMatrix prepos(sn, n_thres);
  double gbeste = 0.0;
  Rcpp::NumericVector prepe(sn);
  Rcpp::IntegerVector ptrail(sn);
  Rcpp::IntegerVector pflags(sn);

  // step 1. generate initial position
  for (int i = 0; i < sn; ++i)
  {
    Rcpp::IntegerVector tmpvec = generate_inipos_multilevel(n_thres, n);
    for (int j = 0; j < n_thres; ++j)
  {
    prepos(i,j) = tmpvec[j];
  }
    prepe[i] = calculate_entropy_multilevel(im_density, im_integral_density, tmpvec);
    if (prepe[i] >= gbeste)
    {
      gbeste = prepe[i];
      for (int j = 0; j < n_thres; ++j)
      {
        gbest[j] = prepos(i,j);
      }
    }
  }  

  for (int cycle = 0; cycle < mcn; ++cycle)
  {
    // step 2. place the employed bees
    for (int j = 0; j < sn; ++j)
    {
    Rcpp::IntegerVector newpos = generate_newpos_multilevel(j, sn, n_thres, n, prepos);
      double newpose = calculate_entropy_multilevel(im_density, im_integral_density, newpos);
      if (newpose >= prepe[j])
      {
        pflags[j] = 1;
        prepe[j] = newpose;
        for (int l = 0; l < n_thres; ++l)
        {
          prepos(j,l) = newpos[l];
        }
      }
    }

    // step 3. Send the outlooker bees (sorting of vector of pair enable us to ----)
    double sum_entropy = 0.0;
    for (int i = 0; i < sn; ++i)
    {
      sum_entropy += prepe[i];
    }
    std::vector<std::pair<int, double> > probs(sn);
    for (int i = 0; i < sn; ++i)
    {
      probs[i].first = i;
      if (sum_entropy != 0.0)
      {
        probs[i].second = prepe[i] / sum_entropy;
      } else 
      {
        probs[i].second = 0.0;
      }
    }
    std::sort(probs.begin(), probs.end(), compare_pairsecond_multilevel);
    for (int i = 0; i < sn; ++i)
    {
      Rcpp::NumericVector temprand = Rcpp::runif(1, 0, 1);
      for (int j = 0; j < sn; ++j)
      {
        if (temprand[0] < probs[j].second)  
        {
          Rcpp::IntegerVector newpos = generate_newpos_multilevel(probs[j].first, sn, n_thres, n, prepos);
          double newpose = calculate_entropy_multilevel(im_density, im_integral_density, newpos);
          if (newpose >= prepe[j])
          {
            pflags[j] = 1;
            prepe[j] = newpose;
            for (int l = 0; l < n_thres; ++l)
            {
            prepos(j,l) = newpos[l];
            }
          }
          break;          
        }
      }
    }
 
    // step 4. Send the scounts
    for (int i = 0; i < sn; ++i)
    {
      if (pflags[i] == 0)
      {
        ptrail[i] += 1;
      }
    }
    double flag_scout = false;
    for (int i = 0; i < sn; ++i)
    {
      if (ptrail[i] > limit)
      {
        flag_scout = true;
        break;
      }
    }
    if (flag_scout)
    {
      Rcpp::IntegerVector pmax(n_thres);
      Rcpp::IntegerVector pmin(n_thres);
      for (int i = 0; i < n_thres; ++i)
      {
        pmax[i] = prepos(0,i);
        pmin[i] = prepos(0,i);
      }
      for (int i = 1; i < sn; ++i)
      {
        for (int j = 0; j < n_thres; ++j)
        {
          if (prepos(i,j) > pmax[j])
          {
            pmax[j] = prepos(i,j);
          }
          if (prepos(i,j) < pmin[j])
          {
            pmin[j] = prepos(i,j);
          }
        }
      }
      for (int i = 0; i < sn; ++i)
      {
        if (ptrail[i] > limit)
        {
          Rcpp::IntegerVector newpos(n_thres);
          Rcpp::NumericVector tmpunif = Rcpp::runif(n_thres, 0, 1);
          for (int j = 0; j < n_thres; ++j)
          {
            int tmppos = (int)(prepos(i,j) + tmpunif[j] * (pmax[j] - pmin[j]));
            if (tmppos < 0)
            {
              tmppos = 0;
            } else if (tmppos >= n)
            {
              tmppos = n - 1;
            }
            newpos[j] = tmppos;
          }
          std::sort(newpos.begin(), newpos.end());
          bool flag_dupl = check_dupl_multilevel(newpos);
          while (flag_dupl)
          {
            tmpunif = Rcpp::runif(n_thres, 0, 1);
            for (int j = 0; j < n_thres; ++j)
            {
              int tmppos = (int)(prepos(i,j) + tmpunif[j] * (pmax[j] - pmin[j]));
              if (tmppos < 0)
              {
                tmppos = 0;
              } else if (tmppos >= n)
              {
                tmppos = n - 1;
              }
              newpos[j] = tmppos;
            }
            std::sort(newpos.begin(), newpos.end());
            flag_dupl = check_dupl_multilevel(newpos);
          }
          double newpose = calculate_entropy_multilevel(im_density, im_integral_density, newpos);
          if (newpose >= prepe[i])
          {
            ptrail[i] = 0;
            prepe[i] = newpose;
            for (int l = 0; l < n_thres; ++l)
            {
              prepos(i,l) = newpos[l];
            }
          }
        }
      }
    }

    // step 5. Record the best solution
    for (int i = 0; i < sn; ++i)
    {
      if (prepe[i] >= gbeste)
      {
        gbeste = prepe[i];
        for (int j = 0; j < n_thres; ++j)
        {
          gbest[j] = prepos(i,j);
        }
      }
    }

  }
  for (int i = 0; i < n_thres; ++i)
  {
    gbest[i] += 1;
  }
  return gbest;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix threshold_multilevel(Rcpp::NumericMatrix im, Rcpp::NumericVector thresvals)
{
  int nrow = im.nrow();
  int ncol = im.ncol();
  int n_thres = thresvals.size();
  Rcpp::NumericMatrix res(nrow, ncol);
  for (int i = 0; i < nrow; ++i)
  {
    for (int j = 0; j < ncol; ++j)
    {
      bool flag = true;
      for (int k = 0; k < n_thres; ++k)
      {
        if (im(i,j) <= thresvals[k])
        {
          flag = false;
          res(i,j) = k;
          break;
        }
      }
      if (flag)
      {
        res(i,j) = n_thres;
      }
    }
  }
  return res;
}