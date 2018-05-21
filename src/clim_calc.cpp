// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"
using namespace arma;
// using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List fun3(arma::mat x, int windowHalfWidth) {
  // number rows and columns
  int ydim = x.n_rows;
  int xdim = x.n_cols;

  // print number of rows and columns
  Rcpp::Rcout << "Rows: " << ydim << std::endl;
  Rcpp::Rcout << "Cols: " << xdim << std::endl;

  arma::vec s_raw(ydim);
  arma::vec t_raw(ydim);
  arma::vec v_raw(ydim);

  for(int i = windowHalfWidth; i < ydim - 1 - windowHalfWidth; i++) {
    // x.submat(row_from, col_from, row_to, col_to)
    arma::mat window = x.submat(i - windowHalfWidth, 0, i + windowHalfWidth, xdim - 1);
    arma::vec v = vectorise(window);
    s_raw[i] = mean(v);
    // t_raw[i] = median(v);
    v_raw[i] = stddev(v);
  }

  // int len_clim_year = 366;
  arma::vec seas = s_raw.subvec(windowHalfWidth, ydim - windowHalfWidth - 1);
  arma::vec var = v_raw.subvec(windowHalfWidth, ydim - windowHalfWidth - 1);

  return Rcpp::List::create(Rcpp::Named("seas") = seas,
                            Rcpp::Named("var") = var);
}
