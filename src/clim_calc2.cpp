#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector fun1(NumericMatrix x, int windowHalfWidth) {
  int nr = x.nrow(), nc = x.ncol();

  NumericVector seas_in = x(_,0);

  // NumericVector seas(nr);
  // NumericVector thresh(nr);
  // NumericVector var(nr);

  return seas_in;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
fun1(ts_wide, 5)
*/
