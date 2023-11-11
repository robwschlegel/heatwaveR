// Smit, A. J.
// 5 November 2023

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector seqDates(int startYear, int startMonth, int startDay, int endYear, int endMonth, int endDay) {
  Date startDate(startYear, startMonth, startDay);
  Date endDate(endYear, endMonth, endDay);
  int len = endDate - startDate + 1;

  IntegerVector out(len);

  for(int i = 0; i < len; ++i) {
    Date newDate = startDate + i;
    out[i] = newDate.getDate();
  }

  return out;
}
