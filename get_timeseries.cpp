#include <Rcpp.h>
#include <array>
#include <vector>

using namespace Rcpp;

// **************************************************************************//
struct Person {

  // ---------------------
  // these need to be in order in the constructor below
  // Personal identifies
  int id;
  int age;

  // IDS
  int household_id;
  int work_id;
  int school_id;
  int community_id;

  //
  bool asymptomatic;
  float internal_viral_mass; // in mass,
  int SEIR_status;

  // ---------------------

  // time-activity
  int get_location(int hour) {

    // ******************
    // if infected stay at home
    if(SEIR_status == 2) {
       return(0);
    }

    // ******************
    // otherwise

    // household
    if(hour < 8 | hour > 20) {
      return(0);
    }

    // work / school
    if(hour > 9 & hour < 17) {
      if(work_id > 0) {
        return(1);
      } else {
        return(2);
      }
    }

    // community
    if(hour == 9 | hour == 17 | hour == 18 | hour == 19) {
      return(3);
    }

    return(-1);

  }

  // method to update your internal viral load
  void update_interal_conc(float C) {

    // probably need to look this up but for now --
    // Source is what you are breathing in until you are infected
    // and then its nothing
    // b = 1/10;              % min/breath,
    // vol_b = 0.5/1000;      % m^3, volume of air breathed in
    float src;
    // ref https://pmc.ncbi.nlm.nih.gov/articles/PMC8672270/
    // 0.45 * 12 = 5.4 L / min = 0.324 m3/hr
    // BUT THIS WILL CHANGE WITH AGE
    float base_inhalation_flux = 0.324; // m^3 / hr
    float age_float = (float) age;

    // imperfect adjustment for age but not terrible for now
    float inhalation_flux = base_inhalation_flux * age_float / 30.0; // m^3 / hr

    // if you are infected or recovered, you cannot increase
    if(SEIR_status == 2 | SEIR_status == 3) {
      src = 0.0;
    } else {
      // src [mass/time] = C [mass/volume] * inhalation_flux [volume/time]
      src = C * inhalation_flux;
    }

    // sink is just the decay rate, [1/time]
    float susceptible_viral_decay = 0.0;    // no antibodies yet
    float exposed_viral_decay     = 0.0;    // no antibodies yet
    float infected_viral_decay    = 0.15;    // this determines recovery time
    float recovered_viral_decay   = 1000.0; // you have antibodies so
    float viral_decay;

    if(SEIR_status == 0) {
      viral_decay = susceptible_viral_decay;
    } else if(SEIR_status == 1) {
      viral_decay = exposed_viral_decay;
    } else if(SEIR_status == 2) {
      viral_decay = infected_viral_decay;
    } else {
      viral_decay = recovered_viral_decay;
    }

    // ok now put it together
    // src = [mass/time]
    // sink = [mass/time]
    float dmdt = src - internal_viral_mass * viral_decay;

    // and update the concentration
    internal_viral_mass = std::max<float>(0, internal_viral_mass + dmdt);

    // as a result of internal mass flux, update SEIR status
    if(internal_viral_mass == 0.0) {
      if(viral_decay == recovered_viral_decay) {
        SEIR_status = 3;
      } else {
        SEIR_status = 0;
      }
    } else if(internal_viral_mass > 0 & internal_viral_mass < 100) {
      SEIR_status = 1;
    } else if(internal_viral_mass >= 100) {
      SEIR_status = 2;
    } else {
      Rcpp::stop("ERROR");
      SEIR_status = -1;
    }

  }

  // get exhalation amount
  float exhalation_mass_flux() {
    // ref https://pmc.ncbi.nlm.nih.gov/articles/PMC8672270/
    // 0.45 * 12 = 5.4 L / min = 0.324 m3/hr
    // BUT THIS WILL CHANGE WITH AGE
    // float exhalation_flux = 0.324; // m^3 / hr

    // the units here get a litte messy
    // but i guess you have to assume that your viral load is what you exhale
    // each hour? ..... ..... .....

    // maybe we can scale this down by some amount to reflect that you
    // dont exhale copies of your entire mass flux every hour
    // probably need to research this somewhat
    return internal_viral_mass / 12.0;
  }

  // method
  void infect() {
    SEIR_status = 2;
    internal_viral_mass = 200;
  }

  void print_self() {
    Rcpp::Rcout << "Person ID# "<< id << ",";
    Rcpp::Rcout << "currentM: " <<  internal_viral_mass << ",";
    Rcpp::Rcout << "SEIR_status: " << SEIR_status << "\n";
  }

  // constructor -- this ensures that when you
  // create the object it has valid properties
  // initialize so 0 internal_viral_mass and infected = False
  Person(int id_,
         int age_,
         int hh_,
         int work_,
         int school_,
         int community_,
         bool asymptomatic_,
         float internal_viral_mass_ = 0.0,
         int SEIR_status_ = 0)
    : id(id_),
      age(age_),
      household_id(hh_),
      work_id(work_),
      school_id(school_),
      community_id(community_),
      asymptomatic(asymptomatic_),
      internal_viral_mass(internal_viral_mass_),
      SEIR_status(SEIR_status_) {}
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

  Rcpp::Rcout << "Initialize\n";

  // Initialize constants
  const int n_hours_per_day = 24;
  const int n_timesteps = n_hours_per_day * n_days;

  // ----------------------------------------------
  // **** ENVIRONMENT CONCENETRATION MATRICES *****
  // ----------------------------------------------
  Rcpp::Rcout << "Concentration matrices\n";
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
  Rcpp::Rcout << "Lookup tables\n";

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

  // only add if the ID is greater than 0
  for (int i = 0; i < n_people; ++i) {
    if(hh_id[i] >= 0) hh_lookup[hh_id[i]].push_back(i);
    if(wk_id[i] >= 0) ww_lookup[wk_id[i]].push_back(i);
    if(ss_id[i] >= 0) ss_lookup[ss_id[i]].push_back(i);
    if(cc_id[i] >= 0) cc_lookup[cc_id[i]].push_back(i);
  }

  // ----------------------------------------------
  // **** AGENT OBJECTS *****
  // ----------------------------------------------
  Rcpp::Rcout << "Agent objects\n";
  std::vector<Person> people;
  people.reserve(n_people);

  for (int i = 0; i < n_people; ++i) {

    // what fraction of people are asymptomatic
    bool asymptomatic = (bool) df(i, 6) == 1;

    people.emplace_back(
      df(i, 0),    // ID
      df(i, 1),    // age
      df(i, 2),    // household_id
      df(i, 3),    // work_id
      df(i, 4),    // school_id
      df(i, 5),    // community_id
      asymptomatic // asymptomatic
    );
  }

  // and someone needs to be the initial infector
  // or maybe some small subset of people, maybe one community
  Rcpp::IntegerVector initial_infectors = Rcpp::wrap(hh_lookup[0]);
  int n_infectors = initial_infectors.size();
  int infect_id;
  Rcpp::Rcout << "Initial infectors\n";
  for(int pi = 0; pi < n_infectors; pi++) {
    infect_id = initial_infectors[pi];
    people[infect_id].infect();
    people[infect_id].print_self();
  }
  Rcpp::Rcout << "\n";
  // ----------------------------------------------
  // **** MAIN LOOP *****
  // ----------------------------------------------
  Rcpp::Rcout << "Main loop\n";
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

  // overall timestep counter
  int timestep = 0;

  // initial concentrations
  float hh_zero = 0;

  // some initial defaults for room circulation
  // Indoor Airflow
  float Q = 0.5 * 3; // volume/time
  // Indoor decay rate
  float k = 0.1; // 1/time

  // initialize concentration
  float C;
  float currE;
  int person_id;

  // loop
  for(int day_i = 0; day_i < n_days; day_i++) {

    Rcpp::Rcout << "********************************"<< "\n";
    Rcpp::Rcout << "\nDAY:  "<< day_i << "\n";


    for(int hour_i = 0; hour_i < n_hours_per_day; hour_i++) {

      Rcpp::Rcout << "HOUR: "<< hour_i << "\n";

      // HOURS ARE defined by the time-activity matrix and you calculate
      // everything at all environments each timestep to account
      // for lingering concentrations

      // ALL HOUSEHOLDS
      // you could also make this a struct as well.
      // for(int hh_i = 0; hh_i < max_hh; hh_i++) {
      for(int hh_i = 0; hh_i < 1; hh_i++) {

        // current concentration
        if(timestep == 0) {
          C = hh_zero;
        } else {
          C = hh_mat(hh_i, timestep - 1);
        }

        // lets get the people in this household;
        // hh_lookup[hh_id[i]] = {1, 3, 5, 6}
        Rcpp::IntegerVector hh_members = Rcpp::wrap(hh_lookup[hh_i]);
        int n_hh_members = hh_members.size();

        // initialize current emission rate;
        // and this is [mass/time] emitted
        // but is also a function of where the person is
        // household is location 0, so check that first then augment the mass flux
        currE = 0;
        for(int pi = 0; pi < n_hh_members; pi++) {
          person_id = hh_members[pi];
          if(people[person_id].get_location(hour_i) == 0) {
            currE = currE + people[person_id].exhalation_mass_flux();
          }
        }
        Rcpp::Rcout << "("<< currE << ")\n";

        // change: Sources (breathing) - Sinks (Airflow and decay rates)
        float dcdt = currE / hh_V - C * (Q / hh_V + k);

        // Rcpp::Rcout << "("<< dcdt << ")\t";

        // augment and set the floor
        hh_mat(hh_i, timestep) = std::max<float>(0.0, C + dcdt);

        // then update the status of each person
        for(int pi = 0; pi < n_hh_members; pi++) {
          person_id = hh_members[pi];
          if(people[person_id].get_location(hour_i) == 0) {
            people[person_id].update_interal_conc(hh_mat(hh_i, timestep));
            people[person_id].print_self();
          }
        }

      } // end household loop

      // ITERATE
      timestep++;

    }
  }



  return List::create(
    _["hh"] = hh_mat
  );

}
