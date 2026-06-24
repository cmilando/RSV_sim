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

//   !--------------------------------------------------------------------------
//   ! Get ids
//   !
//   !--------------------------------------------------------------------------

// [[Rcpp::export]]
Rcpp::List get_ids_cpp(
    Rcpp::NumericMatrix df,
    int n,
    int age_col,
    double age_lower,
    double age_upper,
    int target_col
) {

  // candidate rows
  int nrows = df.nrow();
  std::vector<int> candidate_rows;
  candidate_rows.reserve(nrows);

  // ---------------------------------------------------------
  // Find eligible rows
  // ---------------------------------------------------------

  for (int i = 0; i < nrows; i++) {

    if (df(i, age_col) >= age_lower &&
        df(i, age_col) <= age_upper &&
        df(i, target_col) == 0.0) {
      candidate_rows.push_back(i);
    }
  }

  int m = candidate_rows.size();

  // ---------------------------------------------------------
  // LOOP
  // ---------------------------------------------------------

  if (n >= m) {

    // Not enough remaining observations
    Rcpp::Rcout << "Reached the end\n";

    // convert to an integer vector
    Rcpp::IntegerVector xrows = Rcpp::wrap(candidate_rows);

    // return the list
    return Rcpp::List::create(
      Rcpp::_["rows"] = xrows,
      Rcpp::_["continue"] = false
    );

  } else {

    // Sample n rows

    // convert to an integer vecotr
    Rcpp::IntegerVector xrows = Rcpp::wrap(candidate_rows);

    // get a random sample without replacement
    Rcpp::IntegerVector idx = ransam_cpp(xrows, n);

    // return the list
    return Rcpp::List::create(
      Rcpp::_["rows"] = idx,
      Rcpp::_["continue"] = true
    );

  }
}

//   !--------------------------------------------------------------------------
//   ! Set ids
//   !--------------------------------------------------------------------------

// [[Rcpp::export]]
NumericMatrix set_ids_cpp(
    NumericMatrix df,
    IntegerVector vec,
    int age_col,
    int target_col,
    double p1,
    double p2,
    double age0,
    double age1,
    double age2
) {

  // intiliaze
  bool continue_loop = true;
  int ii = 1;

  // while loop
  while (continue_loop) {

    //
    if (ii % 1000 == 0)
      Rcpp::Rcout << ii << "\t";

    // get this total group size
    int total_grp_size = vec[ii - 1];

    // and now each individual group size
    int grp1_size = std::floor(p1 * total_grp_size);
    int grp2_size = std::ceil(p2 * total_grp_size);

    if(!(grp1_size + grp2_size == total_grp_size)) {
      Rcpp::Rcout << "grp1size: " << grp1_size << "\n";
      Rcpp::Rcout << "grp2size: " << grp2_size << "\n";
      Rcpp::Rcout << "total_grp_size: " << total_grp_size << "\n";
      Rcpp::stop("Error in split math");
    }

    // Initialize
    IntegerVector grp1_rows;
    IntegerVector grp2_rows;
    bool local_continue;
    bool local_continue2;

    // ****************************
    // GROUP 1
    // ****************************
    if (grp1_size > 0) {

      // Rcpp::Rcout << "Grp1\n";

      // get the rows to select
      List out1 = get_ids_cpp(df, grp1_size, age_col,
                              age0, age1 - 1, target_col);

      // get the output from get_ids
      grp1_rows = out1["rows"];
      local_continue = out1["continue"];

    } else {

      // there is nothing in this group
      grp1_rows = IntegerVector(0);
      local_continue = true;

    }

    // ****************************
    // GROUP 2
    // ****************************
    if(local_continue) {

      // Rcpp::Rcout << "Grp2\n";

      // get the rows to select
      List out2 = get_ids_cpp(df, grp2_size, age_col,
                              age1, age2, target_col);

      // get the output from get_ids
      grp2_rows = out2["rows"];
      local_continue2 = out2["continue"];

      if(local_continue2) {

        // Rcpp::Rcout << "setting new row ids\n";

        // set the target col to ii
        for (int i = 0; i < grp1_rows.size(); i++) {
          df(grp1_rows[i], target_col) = ii;
        }

        for (int i = 0; i < grp2_rows.size(); i++) {
          df(grp2_rows[i], target_col) = ii;
        }

        // iterate
        ii++;

        // check stopping condition
        bool done = true;

        for (int r = 0; r < df.nrow(); r++) {

          if (df(r, target_col) == 0) {
            done = false;
            break;
          }
        }

        if (done) {
          Rcpp::Rcout << "All ids are set -- stopping\n";
          continue_loop = false;
        }

      } else {
        Rcpp::Rcout << "out2 continue is FALSE - " <<
          total_grp_size << " - " <<  grp1_size << " - " <<  grp2_size <<"- \n";
        continue_loop = false;
      }

    } else {
      Rcpp::Rcout << "out1 continue is FALSE -" << grp1_size << "- \n";
      continue_loop = false;
    }

  }

  // cleanup
  Rcpp::Rcout << "Number of groups:" << ii  <<"\n";
  Rcpp::Rcout << "Last group size:" << vec[ii] << "\n";

  // ---------------------------------------------------------
  // Assign remaining (-1) individuals into households
  // ---------------------------------------------------------

  std::vector<int> rr;

  double max_id = 0.0;

  for (int i = 0; i < df.nrow(); i++) {

    if (df(i, target_col) == 0)
      rr.push_back(i);

    if (df(i, target_col) > max_id)
      max_id = df(i, target_col);
  }

  int n_remaining = rr.size();

  if (n_remaining > 0) {

    // Build group sizes:
    // rep(c(2,1), floor(n_remaining/3))

    std::vector<int> n_grps;

    int n_triplets = n_remaining / 3;

    for (int i = 0; i < n_triplets; i++) {
      n_grps.push_back(2);
      n_grps.push_back(1);
    }

    int remainder = n_remaining % 3;

    if (remainder > 0)
      n_grps.push_back(remainder);

    // safety check

    int total = 0;
    for (int g : n_grps)
      total += g;

    if (total != n_remaining)
      Rcpp::stop("group size mismatch");

    // randomize rows

    IntegerVector rand_idx =
      Rcpp::sample(n_remaining, n_remaining, false);

    std::vector<int> rand_rows(n_remaining);

    for (int i = 0; i < n_remaining; i++)
      rand_rows[i] = rr[rand_idx[i] - 1];

    // assign household ids

    int hh_id = static_cast<int>(max_id) + 1;

    int rr_i = 0;

    for (size_t i = 0; i < n_grps.size(); i++) {

      int this_grp_size = n_grps[i];

      for (int j = 0; j < this_grp_size; j++) {

        df(rand_rows[rr_i + j], target_col) = hh_id;
      }

      hh_id++;
      rr_i += this_grp_size;
    }
  }

  return df;
}
