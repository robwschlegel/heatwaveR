#include <Rcpp.h>
using namespace Rcpp;

// run length encoding
// input:
//    character vector
// output:
//    character matrix:
//      column 1: value making up the run
//      column 2: start index of the run of values in column 1
//      column 3: end index of the run of values in column 1
//      column 4: length of the run of values in column 1

// [[Rcpp::export]]
CharacterMatrix rle2_char(CharacterVector x) {

  int n = x.size();

  int rows = 1;

  for (int a = 1; a < n; ++a)
    if (x(a) != x(a - 1))
      rows += 1;
  CharacterMatrix out(rows, 4);
  String presval = x(0);
  String nextval = x(0);
  int prespos = 0;
  int index = -1;
  for (int b = 1; b < n; ++b)
  {
    nextval = x(b);
    if (nextval != presval)
    {
      index += 1;
      out(index, 0) = presval;
      out(index, 1) = prespos + 1;
      out(index, 2) = b;
      out(index, 3) = b - prespos;
      presval = nextval;
      prespos = b;
    }
      }
      index += 1;
      out(index,0) = presval;
      out(index,1) = prespos+1;
      out(index,2) = n;
      out(index,3) = n-prespos;
      return(out);
    }
