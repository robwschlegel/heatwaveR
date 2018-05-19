# include "Rcpp.h"
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector mw_cpp(DataFrame d) {
  NumericVector dte = d["t"];

  // the length of the time vector
  int n = dte.size();

  // define and preallocate variables
  Date d_out(0);
  NumericVector d_doy(n);

  // iterator
  for (int i = 0; i < n; i++) {
    d_out = dte(i);
    d_doy[i] = d_out.getYearday();
  }

  // min and max dates
  Date d_min = Date(Rcpp::min(dte));
  Date d_max = Date(Rcpp::max(dte));

  return d_doy;
}


// // [[Rcpp::export]]
// NumericVector mw_seq_cpp(DataFrame d) {
//   NumericVector dte = d["t"];
//   int n = dte.size();
//   Date d_min = Date(Rcpp::min(dte));
//   Date d_max = Date(Rcpp::max(dte));
//   NumericVector out(d_min, d_max);
//   return out;
// }
