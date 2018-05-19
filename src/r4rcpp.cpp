# include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//[[Rcpp::export()]]

int sumCpp(Rcpp::IntegerVector x) {
  int n = x.size();
  int res = 0;
  for (int i = 0; i < n; i++) {
    res += x[i];
  }
  return res;
}
