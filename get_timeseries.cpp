#include <Rcpp.h>
#include <array>
#include <vector>

using namespace Rcpp;

// **************************************************************************//
struct Person {

  // Personal identifies
  int id;
  int age;

  // IDS
  int household_id;
  int workplace_id;
  int school_id;
  int community_id;

  //
  int viral_load;
  bool infected;

  // method
  bool is_adult() {
    return age >= 18;
  }

  // method
  void infect() {
    infected = true;
  }

  // constructor -- this ensures that when you
  // create the object it has valid properties
  // initialize so 0 viral_load and infected = False
  Person(int id_,
         int age_,
         int hh_,
         int work_,
         int school_,
         int community_,
         int viral_load_ = 0,
         bool infected_ = false)
    : id(id_),
      age(age_),
      household_id(hh_),
      workplace_id(work_),
      school_id(school_),
      community_id(community_),
      viral_load(viral_load_),
      infected(infected_) {}
};


// **************************************************************************//
// [[Rcpp::export]]
List get_timeseries(
    IntegerMatrix df,
    IntegerMatrix time_activity,
    int n_days,
    int max_hh,
    int max_work,
    int max_school,
    int max_comm,
    float hh_V,
    float work_V,
    float school_V,
    float comm_V
) {

  // Initialize constants
  const int n_hours_per_day = 24;
  const int n_timesteps = n_hours_per_day * n_days;

  // ----------------------------------------------
  // **** ENVIRONMENT CONCENETRATION MATRICES *****
  // ----------------------------------------------
  // get matrices for concentrations in each environment and time
  // rearranging this so the largest dimension is the rows
  Rcpp::NumericMatrix hh_mat(max_hh, n_timesteps);
  Rcpp::NumericMatrix ww_mat(max_work, n_timesteps);
  Rcpp::NumericMatrix ss_mat(max_school, n_timesteps);
  Rcpp::NumericMatrix cc_mat(max_comm, n_timesteps);

  // ----------------------------------------------
  // **** ENVIRONMENT LOOKUP TABLES *****
  // ----------------------------------------------
  // build the lookup tables
  std::vector<std::vector<int>> hh_lookup(max_hh + 1);
  std::vector<std::vector<int>> ww_lookup(max_work + 1);
  std::vector<std::vector<int>> ss_lookup(max_school + 1);
  std::vector<std::vector<int>> cc_lookup(max_comm + 1);

  // use pointers so you don't make a copy of these
  const int* hh_id  = &df(0, 2);
  const int* wk_id  = &df(0, 3);
  const int* ss_id = &df(0, 4);
  const int* cc_id = &df(0, 5);

  const int n_people = df.nrow();

  for (int i = 0; i < n_people; ++i) {
    hh_lookup[hh_id[i]].push_back(i);
    ww_lookup[wk_id[i]].push_back(i);
    ss_lookup[ss_id[i]].push_back(i);
    cc_lookup[cc_id[i]].push_back(i);
  }

  // ----------------------------------------------
  // **** AGENT OBJECTS *****
  // ----------------------------------------------
  std::vector<Person> people;
  people.reserve(n_people);

  for (int i = 0; i < n_people; ++i) {
    people.emplace_back(
      df(i, 0), // ID
      df(i, 1), // age
      df(i, 2), // household_id
      df(i, 3), // work_id
      df(i, 4), // school_id
      df(i, 5)  // community_id
    );
  }



  // ----------------------------------------------
  // **** MAIN LOOP *****
  // ----------------------------------------------

  // overall timestep counter
  int timestep = 0;

  // initial concentrations
  float hh_zero = 0;

  // loop
  for(int day_i = 0; day_i < n_days; day_i++) {

    Rcpp::Rcout << "\nDAY:  "<< day_i << "\n";
    Rcpp::Rcout << "HOUR:  "<< "\t";

    for(int hour_i = 0; hour_i < n_hours_per_day; hour_i++) {

      Rcpp::Rcout << hour_i << "\t";

      // HOURS ARE defined by the time-activity matrix and you calculate
      // everything at all environments each timestep to account
      // for lingering concentrations

      // SOURCES
      // units: # particles / m^3/ hour
      // b = 1/10;              % min/breath,
      // vol_b = 0.5/1000;      % m^3, volume of air breathed in
      // ==> E [mass/time]

      // ROOM PARAMETERS
      // q_air_exch = 10;       %1/hr,     air exchange rate
      // Q = V_f * q_air_exch;  %m^3/hr,   flow rate in and out of the room
      // ==> [volume/time]

      // SINKS: kVC deposition
      // k [1/time] = 0.1

      // Overall equation
      // dcdt = 1/V *  (E - Q * C - k*V*C)
      //      = E / V  - C * (Q / V + k)
      // [mass/volume/time]
      // C = C + dcdt;

      // Source
      float E = 10; // mass/time

      // Airflow
      float Q = 0.5; // volume/time

      // decay rate
      float k = 0.1; // 1/time

      // concentration
      float C;
      float currE;

      // ALL HOUSEHOLDS
      for(int hh_i = 0; hh_i < max_hh; hh_i++) {

        // look up the people in this house hold
        // hh_lookup[hh_id[i]] = {1, 3, 5, 6}
        // these are rows
        // well no not really, these are person IDs

        // current concentration
        if(timestep == 0) {
          C = hh_zero;
        } else {
          C = hh_mat(hh_i, timestep - 1);
        }

        // introduce varying E
        if(hour_i < 8 | hour_i > 17) {
          currE = E;
        } else {
          currE = 0;
        }

        // change: Sources (breathing) - Sinks (Airflow and decay rates)
        float dcdt = currE / hh_V - C * (Q / hh_V + k);

        // Rcpp::Rcout << "("<< dcdt << ")\t";

        hh_mat(hh_i, timestep) = std::max<float>(0.0, C + dcdt);

      }

      // ITERATE
      timestep++;

    }
  }

  return List::create(
    _["hh"] = hh_mat
  );

}
