// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
using namespace arma;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// simple example of creating two matrices and
// returning the result of an operation on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//
// [[Rcpp::export]]
arma::mat fun1(arma::mat x, int windowHalfWidth) {
  // number rows and columns
  int ydim = x.n_rows, xdim = x.n_cols;
  // iterator start and end
  int is = windowHalfWidth, ie = ydim - windowHalfWidth - 1; // shifted left as zero indexed

  // for (i in (windowHalfWidth + 1):((nrow(data) - windowHalfWidth))) {
  //   seas[i] <-
  //     mean(
  //       c(t(data[(i - (windowHalfWidth)):(i + windowHalfWidth), 1:ncol(data)])),
  //       na.rm = TRUE)
  // }

  // x.submat(row_from, col_from, row_to, col_to)
  arma::mat winvec = x.submat(0, 0, 1, xdim-1);

  // arma::vec out(ydim);

  // for(int i = is; i = ie; i++) {
  //   // out[i] = f(input[i]);
  // }

  return Rcpp::wrap(winvec);
}

