#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector make_histogram_ADPHE(const Rcpp::NumericVector& ordered, const Rcpp::NumericVector& interval)
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

// [[Rcpp::export]]
Rcpp::NumericVector find_local_maximum_ADPHE(const Rcpp::NumericVector& hist, int n)
{
  int size = hist.length();
  int nhalf = n / 2;
  int size_minhalf = size - nhalf;
  Rcpp::LogicalVector tmp(size);
  int count = 0;
  std::list< std::pair<int,double> > window;
  std::pair<int,double> max_elem = std::make_pair(0,0.0);
  
  for (int i = 0; i <= nhalf; ++i)
  {
    if (i < size)
    {
      std::pair<int,double> tmp_pair = std::make_pair(i,hist[i]);
      window.push_back(tmp_pair);
      if (hist[i] > max_elem.second)
      {
        max_elem.first = i;
        max_elem.second = hist[i];
      }
    }
  }
  if (max_elem.first == nhalf)
  {
    tmp[nhalf] = true;
    ++count;
  }
  int sizeminhalf = size - nhalf;
  for (int i = nhalf + 1; i < sizeminhalf; ++i)
  {
    int max_range = i + nhalf;
    window.pop_front(); 
    std::pair<int,double> tmp_pair = std::make_pair(max_range,hist[max_range]);
    window.push_back(tmp_pair);
    if (hist[max_range] > max_elem.second)
    {
        max_elem.first = max_range;
        max_elem.second = hist[max_range];
    }
    if (max_elem.first < i - nhalf)
    {
      std::pair<int,double> tmp_first = *(window.begin());
      max_elem.first = tmp_first.first;
      max_elem.second = tmp_first.second;
      for (std::list< std::pair<int,double> >::iterator itr = window.begin(); itr != window.end(); ++itr)
      {
        std::pair<int,double> tmp_pair = *itr;
        if (tmp_pair.second > max_elem.second)
        {
          max_elem.first = tmp_pair.first;
          max_elem.second = tmp_pair.second;
        }
      }
    }
    if (max_elem.first == i)
    {
      tmp[i] = true;
      ++count;
    }
  }
  
  Rcpp::NumericVector res(count);
  if (count > 0)
  {
    int tmp_count = 0;
    for (int i = 0; i < size; ++i)
    {
      if (tmp[i])
      {
        res[tmp_count] = hist[i];
        ++tmp_count;
      }
    }
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::NumericVector modify_histogram_ADPHE(const Rcpp::NumericVector& imhist, double t_down, double t_up)
{
  int len = imhist.length();
  Rcpp::NumericVector res(len);
  
  for (int i = 0; i < len; ++i)
  {
    if (imhist[i] == 0)
    {
      res[i] = 0;
    } else if (imhist[i] <= t_down)
    {
      res[i] = t_down;
    } else if (imhist[i] < t_up)
    {
      res[i] = imhist[i];
    } else
    {
      res[i] = t_up;
    }
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::NumericVector histogram_equalization_ADPHE(const Rcpp::NumericMatrix& im, const Rcpp::NumericVector& interval2, const Rcpp::NumericVector& imhist_modified, double min_range, double max_range)
{
  int nrow = im.nrow();
  int ncol = im.ncol();
  int len = imhist_modified.length();
  Rcpp::NumericMatrix res(nrow,ncol);
  Rcpp::NumericVector cumulative(len);
  cumulative[0] = 0;
  for (int i = 1; i < len; ++i)
  {
    cumulative[i] = cumulative[i-1] + imhist_modified[i];
  }
  double fm = cumulative[len-1] != 0 ? cumulative[len-1] : 1;
  Rcpp::NumericVector hist_equalized(len);
  for (int i = 0; i < len; ++i)
  {
    hist_equalized[i] = (max_range - min_range) * cumulative[i] / fm + min_range;
  }
  
  for (int i = 0; i < nrow; ++i)
  {
    for (int j = 0; j < ncol; ++j)
    {
      int tmp_k = 0;
      double tmp_k_ratio = 0;
      for (int l = 0; l < len; ++l)
      {
        if (im(i,j) < interval2[l])
        {
          tmp_k = l;
          double tmp_min_range = l > 0 ? interval2[l-1] : 0;
          if (interval2[l] - tmp_min_range != 0)
          {
            tmp_k_ratio = (im(i,j) - tmp_min_range) / (interval2[l] - tmp_min_range);
          } else
          {
            tmp_k_ratio = 0;
          }
          break;
        }
      }
      double tmp_min = tmp_k > 0 ? hist_equalized[tmp_k-1] : 0;
      res(i,j) = tmp_k_ratio * (hist_equalized[tmp_k] - tmp_min) + tmp_min;
    }
  }
  return res;
}