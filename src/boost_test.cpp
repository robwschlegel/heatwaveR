// We can now use the BH package
// [[Rcpp::depends(BH)]]

#include <RcppArmadillo.h>
#include <boost/math/distributions/normal.hpp>
// #include <boost/math/common_factor.hpp>

// using namespace arma;
using namespace Rcpp;

// [[Rcpp::export]]
double cquantile(arma::vec x, double p) {
  double q = boost::math::quantile(x, p);
  return q;
}

// // [[Rcpp::export]]
// int computeGCD(int a, int b) {
//   return boost::math::gcd(a, b);
// }
//
// // [[Rcpp::export]]
// int computeLCM(int a, int b) {
//   return boost::math::lcm(a, b);
// }

// // [[Rcpp::export]]
// arma::vec fun4(arma::mat x, int windowHalfWidth) {
//   // number rows and columns
//   int ydim = x.n_rows;
//   int xdim = x.n_cols;
//
//   arma::vec thresh(ydim);
//
//   arma::mat window = x.submat(5 - windowHalfWidth, 0, 5 + windowHalfWidth, xdim - 1);
//   arma::vec v = vectorise(window);
//   thresh = median(v);
//
//   // int len_clim_year = 366;
//   arma::vec var = v.subvec(windowHalfWidth, ydim - windowHalfWidth - 1);
//
//   return var;
// }

