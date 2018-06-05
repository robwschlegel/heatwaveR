#include <Rcpp.h>
using namespace Rcpp;

// found at:
// https://stackoverflow.com/questions/28442582/reproducing-r-rep-with-the-times-argument-in-c-and-rcpp
//
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
NumericVector rep_vec(const NumericVector& x, const IntegerVector& times) {
  std::size_t n = times.size();
  if (n != 1 && n != x.size())
    stop("Invalid 'times' value");
  std::size_t n_out = std::accumulate(times.begin(), times.end(), 0);
  NumericVector res = no_init(n_out);
  auto begin = res.begin();
  for (std::size_t i = 0, ind = 0; i < n; ind += times[i], ++i) {
    auto start = begin + ind;
    auto end = start + times[i];
    std::fill(start, end, x[i]);
  }
  return res;
}
