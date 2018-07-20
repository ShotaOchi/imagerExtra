//$ @references Faisal Shafait, Daniel Keysers, Thomas M. Breuel, "Efficient implementation of local adaptive thresholding techniques using integral images", Proc. SPIE 6815, Document Recognition and Retrieval XV, 681510 (28 January 2008)

#include <Rcpp.h>

Rcpp::NumericMatrix calc_integralsum(Rcpp::NumericMatrix mat) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  Rcpp::NumericMatrix res(nrow, ncol);

  res(0,0) = mat(0,0);
  for (int i = 1; i < nrow; ++i) {
    res(i,0) = mat(i,0) + res(i-1,0);
  }
  for (int j = 1; j < ncol; ++j) {
    res(0,j) = mat(0,j) + res(0,j-1);
  }
  for (int i = 1; i < nrow; ++i) {
    for (int j = 1; j < ncol; ++j) {
      res(i,j) = mat(i,j) + res(i-1,j) + res(i,j-1) - res(i-1,j-1);
    }
  }
  return res;
}

Rcpp::NumericMatrix calc_integralsum_squared(Rcpp::NumericMatrix mat) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  Rcpp::NumericMatrix mat_squared(nrow, ncol);
  Rcpp::NumericMatrix res(nrow, ncol);

  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < ncol; ++j) {
      mat_squared(i,j) = mat(i,j) * mat(i,j);
    }
  }

  res(0,0) = mat_squared(0,0);
  for (int i = 1; i < nrow; ++i) {
    res(i,0) = mat_squared(i,0) + res(i-1,0);
  }
  for (int j = 1; j < ncol; ++j) {
    res(0,j) = mat_squared(0,j) + res(0,j-1);
  }
  for (int i = 1; i < nrow; ++i) {
    for (int j = 1; j < ncol; ++j) {
      res(i,j) = mat_squared(i,j) + res(i-1,j) + res(i,j-1) - res(i-1,j-1);
    }
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix threshold_adaptive(Rcpp::NumericMatrix mat, double k, int windowsize, double maxsd) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  Rcpp::NumericMatrix res(nrow, ncol);
  Rcpp::NumericMatrix integralsum = calc_integralsum(mat);
  Rcpp::NumericMatrix integralsum_squared = calc_integralsum_squared(mat);
  int winhalf = windowsize / 2;
  int winsize_squared = windowsize * windowsize;
  int nrow_center = nrow - windowsize;
  int ncol_center = ncol - windowsize;

  // sanity check for windowsize
  if (windowsize < 1) {
    Rcpp::Rcout << "Error: window size must be positive." << std::endl;
    return res;
  }
  // sanity check for windowsize and matsize
  if (nrow < windowsize || ncol < windowsize) {
    Rcpp::Rcout << "Error: windowsize is too large." << std::endl;
    return res;
  }
  // sanity check for maxsd
  if (maxsd == 0.0) {
    Rcpp::Rcout << "Error: maxsd is 0." << std::endl;
    return res;
  }
  // sanity check for k
  if (k < 0.0 || k > 1.0) {
    Rcpp::Rcout << "Error: k is out of range. k must be in [0,1]." << std::endl;
    return res;
  }
  
  for (int i = 0; i < winhalf; ++i) {
    for (int j = 0; j < winhalf; ++j) {
      int temp_winsize = (winhalf + i + 1) * (winhalf + j + 1);
      double mean_local = integralsum(i+winhalf,j+winhalf) / temp_winsize;
      double sd_local = sqrt(integralsum_squared(i+winhalf,j+winhalf) / temp_winsize - mean_local * mean_local);
      double threshold_local  = mean_local * (1 + k * (sd_local / maxsd - 1));
      if (mat(i,j) <= threshold_local) {
        res(i,j) = 0;
      } else {
        res(i,j) = 1;
      }
    }
  }

  for (int i = winhalf; i < nrow_center; ++i) {
    for (int j =0; j < winhalf; ++j) {
      int temp_winsize = windowsize * (winhalf + j + 1);
      double mean_local = (integralsum(i+winhalf,j+winhalf) - integralsum(i-winhalf,j+winhalf)) / temp_winsize;
      double sd_local = sqrt((integralsum_squared(i+winhalf,j+winhalf) - integralsum_squared(i-winhalf,j+winhalf)) / temp_winsize - mean_local * mean_local);
      double threshold_local  = mean_local * (1 + k * (sd_local / maxsd - 1));
      if (mat(i,j) <= threshold_local) {
        res(i,j) = 0;
      } else {
        res(i,j) = 1;
      }
    }
  }

  for (int i = nrow_center; i < nrow; ++i) {
    for (int j = 0; j < winhalf; ++j) {
      int temp_winsize = (winhalf + nrow - i) * (winhalf + j + 1);
      double mean_local = (integralsum(nrow-1,j+winhalf) - integralsum(i-winhalf,j+winhalf)) / temp_winsize;
      double sd_local = sqrt((integralsum_squared(nrow-1,j+winhalf) - integralsum_squared(i-winhalf,j+winhalf)) / temp_winsize - mean_local * mean_local);
      double threshold_local = mean_local * (1 + k * (sd_local / maxsd - 1));
      if (mat(i,j) <= threshold_local) {
        res(i,j) = 0;
      } else {
        res(i,j) = 1;
      }
    }
  }

  for (int i = 0; i < winhalf; ++i) {
    for (int j = winhalf; j < ncol_center; ++j) {
      int temp_winsize = (winhalf + i + 1) * windowsize;
      double mean_local = (integralsum(i+winhalf,j+winhalf) - integralsum(i+winhalf,j-winhalf)) / temp_winsize;
      double sd_local = sqrt((integralsum_squared(i+winhalf,j+winhalf) - integralsum_squared(i+winhalf,j-winhalf)) / temp_winsize - mean_local * mean_local);
      double threshold_local = mean_local * (1 + k * (sd_local / maxsd - 1));
      if (mat(i,j) <= threshold_local) {
        res(i,j) = 0;
      } else {
        res(i,j) = 1;
      }
    }
  }

  for (int i = winhalf; i < nrow_center; ++i) {
    for (int j = winhalf; j < ncol_center; ++j) {
      double mean_local = (integralsum(i+winhalf,j+winhalf) + integralsum(i-winhalf,j-winhalf) - integralsum(i+winhalf,j-winhalf) - integralsum(i-winhalf,j+winhalf)) / winsize_squared;
      double sd_local = sqrt((integralsum_squared(i+winhalf,j+winhalf) + integralsum_squared(i-winhalf,j-winhalf) - integralsum_squared(i+winhalf,j-winhalf) - integralsum_squared(i-winhalf,j+winhalf)) / winsize_squared - mean_local * mean_local);
      double threshold_local = mean_local * (1 + k * (sd_local / maxsd - 1));
      if (mat(i,j) <= threshold_local) {
        res(i,j) = 0;
      } else {
        res(i,j) = 1;
      }
    }
  }

  for (int i = nrow_center; i < nrow; ++i) {
    for (int j = winhalf; j < ncol_center; ++j) {
      int temp_winsize = (winhalf + nrow - i) * windowsize;
      double mean_local = (integralsum(nrow-1,j+winhalf) + integralsum(i-winhalf,j-winhalf) - integralsum(nrow-1,j-winhalf) - integralsum(i-winhalf,j+winhalf)) / temp_winsize;
      double sd_local = sqrt((integralsum_squared(nrow-1,j+winhalf) + integralsum_squared(i-winhalf,j-winhalf) - integralsum_squared(nrow-1,j-winhalf) - integralsum_squared(i-winhalf,j+winhalf)) / temp_winsize - mean_local * mean_local);
      double threshold_local = mean_local * (1 + k * (sd_local / maxsd - 1));
      if (mat(i,j) <= threshold_local) {
        res(i,j) = 0;
      } else {
        res(i,j) = 1;
      }
    }
  }

  for (int i = 0; i < winhalf; ++i) {
    for (int j = ncol_center; j < ncol; ++j) {
      int temp_winsize = (winhalf + i + 1) * (winhalf + ncol - j);
      double mean_local = (integralsum(i+winhalf,ncol-1) - integralsum(i+winhalf,j-winhalf)) / temp_winsize;
      double sd_local = sqrt((integralsum_squared(i+winhalf,ncol-1) - integralsum_squared(i+winhalf,j-winhalf)) / temp_winsize - mean_local * mean_local);
      double threshold_local = mean_local * (1 + k * (sd_local / maxsd - 1));
      if (mat(i,j) <= threshold_local) {
        res(i,j) = 0;
      } else {
        res(i,j) = 1;
      }
    }
  }

  for (int i = winhalf; i < nrow_center; ++i) {
    for (int j = ncol_center; j < ncol; ++j) {
      int temp_winsize = windowsize * (winhalf + ncol - j);
      double mean_local = (integralsum(i+winhalf,ncol-1) + integralsum(i-winhalf,j-winhalf) - integralsum(i+winhalf,j-winhalf) - integralsum(i-winhalf,ncol-1)) / temp_winsize;
      double sd_local = sqrt((integralsum_squared(i+winhalf,ncol-1) + integralsum_squared(i-winhalf,j-winhalf) - integralsum_squared(i+winhalf,j-winhalf) - integralsum_squared(i-winhalf,ncol-1)) / temp_winsize - mean_local * mean_local);
      double threshold_local = mean_local * (1 + k * (sd_local / maxsd - 1));
      if (mat(i,j) <= threshold_local) {
        res(i,j) = 0;
      } else {
        res(i,j) = 1;
      }
    }
  }

  for (int i = nrow_center; i < nrow; ++i) {
    for (int j = ncol_center; j < ncol; ++j) {
      int temp_winsize = (winhalf + nrow - i) * (winhalf + ncol - j);
      double mean_local = (integralsum(nrow-1,ncol-1) + integralsum(i-winhalf,j-winhalf) - integralsum(nrow-1,j-winhalf) - integralsum(i-winhalf,ncol-1)) / temp_winsize;
      double sd_local = sqrt((integralsum_squared(nrow-1,ncol-1) + integralsum_squared(i-winhalf,j-winhalf) - integralsum_squared(nrow-1,j-winhalf) - integralsum_squared(i-winhalf,ncol-1)) / temp_winsize - mean_local * mean_local);
      double threshold_local = mean_local * (1 + k * (sd_local / maxsd - 1));
      if (mat(i,j) <= threshold_local) {
        res(i,j) = 0;
      } else {
        res(i,j) = 1;
      }
    }
  }

  return res;
}