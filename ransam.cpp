#include <Rcpp.h>
using namespace Rcpp;

//   !--------------------------------------------------------------------------
//   ! Subroutine for random sampling without replacement, and goes
//   ! only once through the dataset
//   !
//   ! from 1977: https://link.springer.com/content/pdf/10.3758/BF03214009.pdf
//   !   x = all_potential_rows
//   !   subx = this_sample
//   !   n = size of x
//   !   k = size of subx
//   !--------------------------------------------------------------------------

// [[Rcpp::export]]
IntegerVector ransam_cpp(IntegerVector x, int k) {

  int n = x.size();

  if (k > n)
    stop("k cannot be greater than length(x)");

  // Initialize subx with size k
  // initialize m at -1 to offset for c++ being 0-indexed
  IntegerVector subx(k);
  int m = -1;

  for (int j = 0; j < n; j++) {

    // generate a random number
    double r = R::runif(0.0, 1.0);

    // get the L
    int L = (int)((n - j + 1) * r + 1);

    // compare L to k and m
    if (L <= (k - m)) {

      // so add +1
      m++;

      // move the break statement up so it ends if
      // m is greater than the size of x
      if (m >= k)
        break;

      // otherwise increment
      subx[m] = x[j];

    } // end if
  } // end loop

  return subx;
}



