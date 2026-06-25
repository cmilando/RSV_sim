#include <Rcpp.h>
#include <array>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame contact_matrix_cpp(
    IntegerVector age,
    NumericVector household_id,
    int max_hh,
    NumericVector work_id,
    int max_work,
    NumericVector school_id,
    int max_school,
    NumericVector community_id,
    int max_comm
) {

  // Initialize
  const int N = age.size();
  const int NAGE = 101;      // from 0 to 100

  //--------------------------------------------------
  // Populate group age distributions
  // this is just the ages of different people in each
  // household, school, etc
  // so these are two
  //--------------------------------------------------

  // Each "group" (household/work/school/community) gets its own age distribution.
  //
  // Structure:
  //   vector index = group ID
  //   array index  = age (0–100)
  //   value        = count of people of that age in the group
  //
  // So:
  //   hh_counts[g][a] = number of people age 'a' in household 'g'
  std::vector<std::array<int,101>> hh_counts(max_hh);
  std::vector<std::array<int,101>> work_counts(max_work);
  std::vector<std::array<int,101>> school_counts(max_school);
  std::vector<std::array<int,101>> comm_counts(max_comm);

  // Initialize all counts to zero.
  //
  // std::array is not automatically zero-initialized in a vector context,
  // so we explicitly fill each group's age histogram.
  for(auto &x : hh_counts)     x.fill(0);
  for(auto &x : work_counts)   x.fill(0);
  for(auto &x : school_counts) x.fill(0);
  for(auto &x : comm_counts)   x.fill(0);

  // Cache raw pointers (avoids repeated SEXP indexing overhead)
  const double* hh_id   = REAL(household_id);
  const double* wk_id   = REAL(work_id);
  const double* sch_id  = REAL(school_id);
  const double* com_id  = REAL(community_id);
  const int* ag         = INTEGER(age);

  // loop over every individual
  for(int i = 0; i < N; i++) {

    // this persons age
    const int a = ag[i];

    // HOUSEHOLD
    if(!ISNA(hh_id[i])) {
      int g = (int) hh_id[i];  // cast the current hh_id to an integer
      hh_counts[g][a]++;       // increment the age count by 1
    }

    // WORK
    if(!ISNA(wk_id[i])) {
      int g = (int) wk_id[i];
      work_counts[g][a]++;
    }

    // SCHOOL
    if(!ISNA(sch_id[i])) {
      int g = (int) sch_id[i];
      school_counts[g][a]++;
    }

    // COMMUNITY
    if(!ISNA(com_id[i])) {
      int g = (int) com_id[i];
      comm_counts[g][a]++;
    }
  }

  //--------------------------------------------------
  // Contact matrices
  //--------------------------------------------------

  NumericMatrix hh(NAGE, NAGE);
  NumericMatrix work(NAGE, NAGE);
  NumericMatrix school(NAGE, NAGE);
  NumericMatrix comm(NAGE, NAGE);

  NumericVector n_ref(NAGE);

  for(int i=0; i<N; i++) {

    int ref_age = age[i];

    // the total number of people in each age group
    // used later for scaling of the mean contacts
    n_ref[ref_age]++;

    // Household
    if(!NumericVector::is_na(household_id[i])) {

      // type cast
      int gid = (int) household_id[i];

      // group add all the counts for each environment
      for(int b=0; b<NAGE; b++)
        hh(ref_age,b) += hh_counts[gid][b];

      // remove self
      hh(ref_age, ref_age)--;
    }


    // Work
    if(!NumericVector::is_na(work_id[i])) {

      // type cast
      int gid = (int) work_id[i];

      for(int b=0; b<NAGE; b++)
        work(ref_age,b) += work_counts[gid][b];

      // remove self
      work(ref_age, ref_age)--;
    }

    // School
    if(!NumericVector::is_na(school_id[i])) {

      // type cast
      int gid = (int) school_id[i];

      for(int b=0; b<NAGE; b++)
        school(ref_age,b) += school_counts[gid][b];

      // remove self
      school(ref_age, ref_age)--;
    }


    // Community
    if(!NumericVector::is_na(community_id[i])) {

      // type cast
      int gid = (int) community_id[i];

      for(int b=0; b<NAGE; b++)
        comm(ref_age,b) += comm_counts[gid][b];

      // remove self
      comm(ref_age, ref_age)--;
    }
  }

  //--------------------------------------------------
  // Convert to long output
  //--------------------------------------------------

  int OUT = NAGE * NAGE;

  IntegerVector ref_age_out(OUT);
  IntegerVector contact_age_out(OUT);

  NumericVector hh_out(OUT);
  NumericVector work_out(OUT);
  NumericVector school_out(OUT);
  NumericVector comm_out(OUT);

  NumericVector total_out(OUT);

  int idx = 0;

  for(int a=0; a<NAGE; a++) {

    // the age specific denominator
    // an ifelse statement
    double denom = (n_ref[a] > 0) ? n_ref[a] : NA_REAL;

    for(int b=0; b<NAGE; b++) {

      // set the ref age
      ref_age_out[idx] = a;

      // set the contact age
      contact_age_out[idx] = b;

      if(R_IsNA(denom)) {

        hh_out[idx]   = NA_REAL;
        work_out[idx] = NA_REAL;
        school_out[idx] = NA_REAL;
        comm_out[idx] = NA_REAL;
        total_out[idx] = NA_REAL;

      } else {

        hh_out[idx]     = hh(a,b) / denom;

        work_out[idx]   = work(a,b) / denom;

        school_out[idx] = school(a,b) / denom;

        comm_out[idx]   = comm(a,b) / denom;

        total_out[idx] =
          (hh(a,b) + work(a,b) + school(a,b) + comm(a,b)) / denom;

      }

      idx++;
    }
  }

  return DataFrame::create(
    _["ref_age"] = ref_age_out,
    _["nref"] = n_ref,
    _["contact_age"] = contact_age_out,
    _["household"] = hh_out,
    _["community"] = comm_out,
    _["school"] = school_out,
    _["work"] = work_out,
    _["total"] = total_out
  );
}
