// Smit, A. J.
// 20 May 2018

#include "RcppArmadillo.h"
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::NumericMatrix clim_calc_cpp(arma::mat x, int windowHalfWidth, double pctile) {

  // number rows and columns
  int ydim = x.n_rows;
  int xdim = x.n_cols;
  int window_vec = ((windowHalfWidth * 2) + 1) * xdim;

  // quantile setup
  int lo, hi;
  double hlo, hhi;
  double index = (window_vec - 1) * (pctile / 100); // window_vec = n; the rank of the quantiles
  lo = floor(index);
  hi = lo + 1;
  hhi = index - lo;
  hlo = 1 - hhi;

  // sequence of numbers for finding quantiles
  arma::vec id(window_vec);
  std::iota(id.begin(), id.end(), 0);

  // allocate receiving vectors
  arma::vec s_raw(ydim); // climatology
  arma::vec t_raw(ydim); // threshold

  for(int i = windowHalfWidth; i < ydim - windowHalfWidth; i++) {
    arma::mat window = x.submat(i - windowHalfWidth, 0, i + windowHalfWidth, xdim - 1);
    arma::vec v1 = vectorise(window);
    arma::vec v2 = v1.elem( find_finite(v1) ); // handle missing values
    arma::vec v3 = sort(v2);
    s_raw[i] = mean(v3);
    t_raw[i] = hlo * v3[id[lo]] + hhi * v3[id[hi]];
  }

  // trim tops and bottoms (return 366 day climatology)
  arma::vec doy(366);
  std::iota(doy.begin(), doy.end(), 1);
  arma::vec seas = s_raw.subvec(windowHalfWidth, ydim - windowHalfWidth - 1);
  arma::vec thresh = t_raw.subvec(windowHalfWidth, ydim - windowHalfWidth - 1);

  // place vectors into a matrix
  arma::mat ret(seas.n_elem, 3);
  ret.col(0) = doy;
  ret.col(1) = seas;
  ret.col(2) = thresh;
  // ret.col(3) = var;

  // convert to Rcpp
  Rcpp::NumericMatrix ans(Rcpp::wrap(ret));

  // and name the columns
  colnames(ans) = Rcpp::CharacterVector::create("doy", "seas", "thresh");

  return ans;
  }
