//$ reference: Makhoul, J. (1980). A fast cosine transform in one and two dimensions. IEEE Transactions on Acoustics, Speech, and Signal Processing. 28 (1): 27-34. 

#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericMatrix DCT2D_reorder(Rcpp::NumericMatrix mat) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  Rcpp::NumericMatrix res(nrow, ncol);

  int nrowp1_half = (nrow + 1) / 2;
  int ncolp1_half = (ncol + 1) / 2;
  for (int i = 0; i < nrowp1_half; ++i) {
    for (int j = 0; j < ncolp1_half; ++j) {
      res(i,j) = mat(2*i, 2*j);
    }
    for (int j = ncolp1_half; j < ncol; ++j) {
      res(i,j) = mat(2*i, 2*ncol-2*j-1);
    }
  }
  for (int i = nrowp1_half; i < nrow; ++i) {
    for (int j = 0; j < ncolp1_half; ++j) {
      res(i,j) = mat(2*nrow-2*i-1, 2*j);
    }
    for (int j = ncolp1_half; j < ncol; ++j) {
      res(i,j) = mat(2*nrow-2*i-1, 2*ncol-2*j-1);
    }
  }
  return res;  
}

Rcomplex calc_wm(int k, double m) {
  double coef = -2 * M_PI * k / m;
  Rcomplex res;
  res.r = cos(coef);
  res.i = sin(coef);
  return res;
} 

//$' calculate DCT2D from DFT2D
// [[Rcpp::export]]
Rcpp::NumericMatrix DCT2D_fromDFT(Rcpp::ComplexMatrix mat) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  double nrow4 = 4.0 * nrow;
  double ncol4 = 4.0 * ncol;
  Rcpp::NumericMatrix res(nrow, ncol);

  for (int j = 0; j < ncol; ++j) {
    int i = 0;
    Rcomplex wk14N1 = calc_wm(i, nrow4);
    Rcomplex wk24N2 = calc_wm(j, ncol4);
    Rcomplex temp = wk14N1 * (wk24N2 * mat(i,j));
    res(i,j) = temp.r;
  }
  for (int i = 1; i < nrow; ++i) {
    Rcomplex wk14N1 = calc_wm(i, nrow4);
    int j = 0;
    Rcomplex wk24N2 = calc_wm(j, ncol4);
    Rcomplex temp = wk14N1 * (wk24N2 * mat(i,j));
    res(i,j) = temp.r;
  }
  for (int i = 1; i < nrow; ++i) {
    Rcomplex wk14N1 = calc_wm(i, nrow4);
    for (int j = 1; j < ncol; ++j) {
      Rcomplex wk24N2 = calc_wm(j, ncol4);
      Rcomplex wmik24N2 = calc_wm(-j, ncol4);
      Rcomplex temp = wk14N1 * (wk24N2 * mat(i,j) + wmik24N2 * mat(i,ncol-j));
      res(i,j) = temp.r * 0.5;
    }
  }
  return res;
}

//$' calculate DFT2D from DCT2D
// [[Rcpp::export]]
Rcpp::ComplexMatrix IDCT2D_toDFT(Rcpp::NumericMatrix mat) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  double nrow4 = 4.0 * nrow;
  double ncol4 = 4.0 * ncol;
  Rcpp::ComplexMatrix res(nrow, ncol);

  res(0,0).r = mat(0,0);
  res(0,0).i = 0.0;
  for (int i = 1; i < nrow; ++i) {
    int j = 0;
    Rcomplex wmik14N1 = calc_wm(-i, nrow4);
    Rcomplex wmik24N2 = calc_wm(-j, ncol4);
    Rcomplex temp;
    temp.r = mat(i,j);
    temp.i = -mat(nrow-i,j);
    Rcomplex temp2 = wmik14N1 * wmik24N2 * temp;
    res(i,j).r = temp2.r;
    res(i,j).i = temp2.i;
  }
  for (int j = 1; j < ncol; ++j) {
    int i = 0;
    Rcomplex wmik14N1 = calc_wm(-i, nrow4);
    Rcomplex wmik24N2 = calc_wm(-j, ncol4);
    Rcomplex temp;
    temp.r = mat(i,j);
    temp.i = -mat(i,ncol-j);
    Rcomplex temp2 = wmik14N1 * wmik24N2 * temp;
    res(i,j).r = temp2.r;
    res(i,j).i = temp2.i;
  }
  for (int i = 1; i < nrow; ++i) {
    Rcomplex wmik14N1 = calc_wm(-i, nrow4);
    for (int j = 1; j < ncol; ++j) {
      Rcomplex wmik24N2 = calc_wm(-j, ncol4);
      Rcomplex temp;
      temp.r = mat(i,j) - mat(nrow-i,ncol-j);
      temp.i = -(mat(nrow-i,j) + mat(i,ncol-j));
      Rcomplex temp2 = wmik14N1 * wmik24N2 * temp;
      res(i,j).r = temp2.r;
      res(i,j).i = temp2.i;
    }
  }

  return res;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix IDCT2D_retrievex(Rcpp::NumericMatrix mat) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  Rcpp::NumericMatrix res(nrow, ncol);

  int nrowp1_half = (nrow + 1) / 2;
  int ncolp1_half = (ncol + 1) / 2;
  for (int i = 0; i < nrowp1_half; ++i) {
    for (int j = 0; j < ncolp1_half; ++j) {
      res(2*i,2*j) = mat(i, j);
    }
    for (int j = ncolp1_half; j < ncol; ++j) {
      res(2*i,2*ncol-2*j-1) = mat(i, j);
    }
  }
  for (int i = nrowp1_half; i < nrow; ++i) {
    for (int j = 0; j < ncolp1_half; ++j) {
      res(2*nrow-2*i-1,2*j) = mat(i,j);
    }
    for (int j = ncolp1_half; j < ncol; ++j) {
      res(2*nrow-2*i-1, 2*ncol-2*j-1) = mat(i,j);
    }
  }
  return res;  
}