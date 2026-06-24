#ifndef header_H
#define header_H

#include <Rcpp.h>

Rcpp::IntegerVector fsubset_cpp(
    Rcpp::NumericMatrix df,
    int age_col,
    double age_lb,
    double age_ub,
    int zero_col
);

Rcpp::IntegerVector ransam_cpp(
    Rcpp::IntegerVector x,
    int k
);

#endif
