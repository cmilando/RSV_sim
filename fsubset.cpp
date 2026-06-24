#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector fsubset_cpp(
    NumericMatrix df,
    int age_col,
    double age_lb,
    double age_ub,
    int zero_col
) {


  int nrows = df.nrow();

  IntegerVector rr(nrows, 0);

  // Convert from R's 1-based indexing to C++ 0-based indexing
  age_col--;
  zero_col--;

  for (int i = 0; i < nrows; i++) {
    if (df(i, age_col) >= age_lb &&
        df(i, age_col) < age_ub &&
        df(i, zero_col) == 0.0) {

      rr[i] = 1;
    }
  }

  return rr;
}
