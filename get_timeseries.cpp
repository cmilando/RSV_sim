#include <Rcpp.h>
#include <array>
#include <vector>
#include <random>

using namespace Rcpp;

// **************************************************************************//
struct Person {

  // ---------------------
  // CONSTANTS
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
  bool stays_home;
  bool is_incident;
  float internal_viral_mass; // in #,
  int SEIR_status;
  //     0 = Susceptible
  //     1 = Exposed
  //     2 = Infected
  //     3 = Recovered
  int location;
  int incidence_location;

  //
  int timesteps_for_incubation;
  int timesteps_for_recovery;

  // ---------------------
  // METHODS
  // ---------------------

  // time-activity
  void set_location(int hour) {

    // ******************
    // if infected, theres an 80% chance you stay at home
    if(SEIR_status == 2) {
      if(stays_home == 1) {
        location = 0;
      }
    }

    // ******************
    // otherwise

    // household
    if(hour < 8 | hour > 20) {
      location = 0;
    }

    // work / school
    if(hour > 9 & hour < 17) {
      if(work_id > 0) {
        location = 1;
      } else {
        location = 2;
      }
    }

    // community
    if(hour == 9 | hour == 17 | hour == 18 | hour == 19) {
      location = 3;
    }

  }

  // method to update your internal viral load
  void update_interal_mass(float C) {

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
    float age_adjustment = (float) age;

    // setting a threshold here
    // this keeps it so that its always positive and between 0.1 and 30
    age_adjustment = std::min<float>(1.0, std::max<float>(0.2, age_adjustment/30.0));

    // imperfect adjustment for age but not terrible for now
    float inhalation_flux = base_inhalation_flux * age_adjustment; // m^3 / hr
    // float inhalation_flux = base_inhalation_flux;

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

    // and update the internal mass
    internal_viral_mass = std::max<float>(0, internal_viral_mass + dmdt);

    // set a lower threshold
    if(internal_viral_mass < 0.01) {
      internal_viral_mass = 0.0;
    }

    // incident is only true if you turn incident this timestep
    is_incident = false;

    // now update SEIR status
    if(SEIR_status == 0) {
      // susceptible -> exposed
      if(internal_viral_mass > 0) {
        SEIR_status = 1;
      }

    } else if(SEIR_status == 1) {
      // exposed -> susceptible
      if(internal_viral_mass == 0.0) {
        SEIR_status = 0;
      }

      // exposed -> infected
      // once you cross some threshold of exposure, you are on the pathway
      // towards being infected, so start counting down
      if(internal_viral_mass > 100.0) {
        timesteps_for_incubation--;
        if(timesteps_for_incubation == 0) {
          SEIR_status = 2;
          is_incident = true; // the only time this occurs
          incidence_location = location;
        }
      }

    } else if(SEIR_status == 2) {
      // infected -> recovered
      timesteps_for_recovery--;
      if(timesteps_for_recovery == 0) {
        SEIR_status = 3;
      }

    } else if(SEIR_status == 3) {
      //

    } else {
      Rcpp::stop("ERROR");
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
         bool stays_home_,
         bool is_incident_ = false,
         float internal_viral_mass_ = 0.0,
         int SEIR_status_ = 0,
         int location_ = 0,
         int incidence_location_ = -1,
         int timesteps_for_incubation_ = 24 * 2, // 24 hours * 2 days
         int timesteps_for_recovery_ = 24 * 3) // 24 hours * 2 days
    : id(id_),
      age(age_),
      household_id(hh_),
      work_id(work_),
      school_id(school_),
      community_id(community_),
      asymptomatic(asymptomatic_),
      stays_home(stays_home_),
      is_incident(is_incident_),
      internal_viral_mass(internal_viral_mass_),
      SEIR_status(SEIR_status_),
      location(location_),
      incidence_location(incidence_location_),
      timesteps_for_incubation(timesteps_for_incubation_),
      timesteps_for_recovery(timesteps_for_recovery_) {}
};


// **************************************************************************//
void update_environment_agents(
  int timestep,
  int this_zone,
  int hour_i,
  float zero_conc,
  int zone_i,
  float zone_V,
  float Q,
  float k,
  // pass by reference because you want it to update
  // and because you don't want it to make copies each time
  std::vector<float>& zone_conc,
  std::vector<std::vector<int>>& zone_lookup,
  std::vector<Person>& people
) {

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

  float zone_C;
  float currE;
  int person_id;

  // current concentration
  if(timestep == 0) {
    zone_C = zero_conc;
  } else {
    zone_C = zone_conc[zone_i];
  }

  // lets get the people in this household;
  // hh_lookup[hh_id[i]] = {1, 3, 5, 6}
  Rcpp::IntegerVector zone_members = Rcpp::wrap(zone_lookup[zone_i]);
  int n_hh_members = zone_members.size();

  // initialize current emission rate;
  // and this is [mass/time] emitted
  // but is also a function of where the person is
  // household is location 0, so check that first then augment the mass flux
  currE = 0;
  for(int pi = 0; pi < n_hh_members; pi++) {
    person_id = zone_members[pi];
    if(people[person_id].location == this_zone) {
      currE = currE + people[person_id].exhalation_mass_flux();
    }
  }

  // change: Sources (breathing) - Sinks (Airflow and decay rates)
  float dcdt = currE / zone_V - zone_C * (Q / zone_V + k);

  // augment and set the floor
  zone_conc[zone_i] = std::max<float>(0.0, zone_C + dcdt);

  // then update the status of each person
  for(int pi = 0; pi < n_hh_members; pi++) {
    person_id = zone_members[pi];
    if(people[person_id].location == this_zone) {
      people[person_id].update_interal_mass(zone_conc[zone_i]);
      // people[person_id].print_self();
    }
  }
}

// **************************************************************************//
// [[Rcpp::export]]
List get_timeseries(
    IntegerMatrix df,
    IntegerMatrix time_activity,
    int n_days,
    float hh_V,
    float work_V,
    float school_V,
    float comm_V,
    IntegerVector personIDs_to_track,
    IntegerVector hhIDs_to_track,
    IntegerVector workIDs_to_track,
    IntegerVector schoolIDs_to_track,
    IntegerVector commIDs_to_track
) {

  Rcpp::Rcout << "Initialize\n";

  // Initialize constants
  const int n_hours_per_day = 24;
  const int n_timesteps = n_hours_per_day * n_days;

  const int n_ages = 100 + 1; //0 to 100

  const int max_hh = Rcpp::max(df(_, 2));
  const int max_work = Rcpp::max(df(_, 3));
  const int max_school = Rcpp::max(df(_, 4));
  const int max_comm = Rcpp::max(df(_, 5));

  const int ntrack_people = personIDs_to_track.size();
  const int ntrack_hh = hhIDs_to_track.size();
  const int ntrack_ww = workIDs_to_track.size();
  const int ntrack_ss = schoolIDs_to_track.size();
  const int ntrack_cc = commIDs_to_track.size();

  // ----------------------------------------------
  // **** ENVIRONMENT CONCENETRATION VECTORS *****
  // ----------------------------------------------
  Rcpp::Rcout << "Environment Concentrations \n";

  // get matrices for concentrations in each environment
  // choosing not to save all concentrations for all times and instead
  // just tracking those below
  std::vector<float> hh_conc(max_hh);
  std::vector<float> ww_conc(max_work);
  std::vector<float> ss_conc(max_school);
  std::vector<float> cc_conc(max_comm);

  // ----------------------------------------------
  // **** TRACKING MATRICES *****
  // ----------------------------------------------
  Rcpp::Rcout << "Track matrices\n";

  // get matrices for concentrations in each environment and time
  // rearranging this so the largest dimension is the rows
  Rcpp::NumericMatrix person_mat(n_timesteps, ntrack_people);
  Rcpp::NumericMatrix seir_mat(n_timesteps, ntrack_people);
  Rcpp::NumericMatrix hh_mat(n_timesteps, ntrack_hh);
  Rcpp::NumericMatrix ww_mat(n_timesteps, ntrack_ww);
  Rcpp::NumericMatrix ss_mat(n_timesteps, ntrack_ss);
  Rcpp::NumericMatrix cc_mat(n_timesteps, ntrack_cc);

  // Also want to track age-specific incidence and prevalence
  // Incidence is number of *new cases* this timestep
  // Prevalence is total number of cases at this time
  Rcpp::NumericMatrix incidence_mat(n_timesteps, n_ages);
  Rcpp::NumericMatrix prevalence_mat(n_timesteps, n_ages);

  // ----------------------------------------------
  // **** NETWORK LOOKUP TABLES *****
  // ----------------------------------------------
  // build the lookup tables
  Rcpp::Rcout << "Lookup tables\n";

  // lists of vectors
  std::vector<std::vector<int>> hh_lookup(max_hh + 1);
  std::vector<std::vector<int>> ww_lookup(max_work + 1);
  std::vector<std::vector<int>> ss_lookup(max_school + 1);
  std::vector<std::vector<int>> cc_lookup(max_comm + 1);

  // use pointers so you don't make a copy of these
  const int n_people = df.nrow();

  // only add if the ID is greater than 0
  for (int i = 0; i < n_people; ++i) {
    if(df(i, 2) >= 0) hh_lookup[df(i, 2)].push_back(i);
    if(df(i, 3) >= 0) ww_lookup[df(i, 3)].push_back(i);
    if(df(i, 4) >= 0) ss_lookup[df(i, 4)].push_back(i);
    if(df(i, 5) >= 0) cc_lookup[df(i, 5)].push_back(i);
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

    // when you are sick do you stay home
    bool stays_home = (bool) df(i, 7) == 1;

    people.emplace_back(
      df(i, 0),     // ID
      df(i, 1),     // age
      df(i, 2),     // household_id
      df(i, 3),     // work_id
      df(i, 4),     // school_id
      df(i, 5),     // community_id
      asymptomatic, // asymptomatic
      stays_home    // stays_home
    );
  }

  // seed the infections
  Rcpp::Rcout << "Initial infectors\n";
  for(int pi = 0; pi < n_people; pi++) {
    if(df(pi, 8) == 1) {
      people[pi].infect();
    }
    //people[infect_id].print_self();
  }

  // ----------------------------------------------
  // **** MAIN LOOP *****
  // ----------------------------------------------
  Rcpp::Rcout << "Main loop\n";

  // overall timestep counter
  int timestep = 0;

  // initial concentrations
  float zero_conc = 0;

  // some initial defaults for room circulation
  // Indoor Airflow
  float Q = 0.5 * 3; // volume/time
  // Indoor decay rate
  float k = 0.1; // 1/time

  // helpers
  int this_id;
  int this_zone;

  // incidence and prevalence
  Rcpp::IntegerVector incidence_sum(n_ages);
  Rcpp::IntegerVector prevalence_sum(n_ages);

  // incidence locations
  Rcpp::IntegerMatrix incidence_location(n_ages, 4);

  Rcpp::Rcout << "********************************"<< "\n";
  Rcpp::Rcout << "DAY: ";

  // loop
  for(int day_i = 0; day_i < n_days; day_i++) {

    Rcpp::Rcout << day_i << "\t";

    for(int hour_i = 0; hour_i < n_hours_per_day; hour_i++) {

      // HOURS ARE defined by the time-activity matrix and you calculate
      // everything at all environments each timestep to account
      // for lingering concentrations

      // reset these each timestep
      for(int ni = 0; ni < n_ages; ni ++) {
        incidence_sum[ni] = 0;
        prevalence_sum[ni] = 0;
      }

      // update everyone's location
      for(int pi = 0; pi < n_people; pi ++) {
        people[pi].set_location(hour_i);
      }

      // ***********************
      // ALL HOUSEHOLDS
      for(int hh_i = 0; hh_i < max_hh; hh_i++) {
        this_zone = 0;
        update_environment_agents(
           timestep, this_zone, hour_i, zero_conc,
           hh_i, hh_V,
           Q = Q, k = k,
           hh_conc, hh_lookup, people
        );

      } // end household loop

      // ***********************
      // ALL WORK
      for(int ww_i = 0; ww_i < max_work; ww_i++) {
        this_zone = 1;
        update_environment_agents(
          timestep, this_zone, hour_i, zero_conc,
          ww_i, work_V,
          Q = Q, k = k,
          ww_conc, ww_lookup, people
        );

      } // end work loop

      // ***********************
      // ALL SCHOOL
      for(int ss_i = 0; ss_i < max_school; ss_i++) {
        this_zone = 2;
        update_environment_agents(
          timestep, this_zone, hour_i, zero_conc,
          ss_i, school_V,
          Q = Q, k = k,
          ss_conc, ss_lookup, people
        );

      } // end school loop

      // ***********************
      // ALL COMMUNITY
      for(int cc_i = 0; cc_i < max_comm; cc_i++) {
        this_zone = 3;
        update_environment_agents(
          timestep, this_zone, hour_i, zero_conc,
          cc_i, comm_V,
          Q = Q, k = k,
          cc_conc, cc_lookup, people
        );

      } // end community loop

      // ***********************
      // now save the ones you are tracking
      // assumes you have one in each
      for(int ti = 0; ti < ntrack_people; ti ++) {
        this_id = personIDs_to_track[ti];
        person_mat(timestep, ti) = people[this_id].internal_viral_mass;
        seir_mat(timestep, ti) = people[this_id].SEIR_status;
      }
      for(int ti = 0; ti < ntrack_hh; ti ++) {
        this_id = hhIDs_to_track[ti];
        hh_mat(timestep, ti) = hh_conc[this_id];
      }
      for(int ti = 0; ti < ntrack_ww; ti ++) {
        this_id = workIDs_to_track[ti];
        ww_mat(timestep, ti) = ww_conc[this_id];
      }
      for(int ti = 0; ti < ntrack_ss; ti ++) {
        this_id = schoolIDs_to_track[ti];
        ss_mat(timestep, ti) = ss_conc[this_id];
      }
      for(int ti = 0; ti < ntrack_cc; ti ++) {
        this_id = commIDs_to_track[ti];
        cc_mat(timestep, ti) = cc_conc[this_id];
      }

      // and the overall incidence and prevalence trackers
      for(int pi = 0; pi < n_people; pi ++) {
        if(people[pi].is_incident) {
          incidence_sum[people[pi].age]++;
        }
        if(people[pi].SEIR_status == 2) {
          prevalence_sum[people[pi].age]++;
        }
      }
      for(int ni = 0; ni < n_ages; ni ++) {
        incidence_mat(timestep, ni) = incidence_sum[ni];
        prevalence_mat(timestep, ni) = prevalence_sum[ni];
      }

      // ITERATE
      timestep++;

    }
  }

  // total the incidence locations
  for(int pi = 0; pi < n_people; pi ++) {
    if(people[pi].incidence_location >= 0) {
      incidence_location(people[pi].age, people[pi].incidence_location)++;
    }
  }

  Rcpp::Rcout << "\n********************************"<< "\n";
  Rcpp::Rcout << "COMPLETE.\n";

  return List::create(
    _["person_mass"]    = person_mat,
    _["person_seir"]    = seir_mat,
    _["household_conc"] = hh_mat,
    _["school_conc"]    = ss_mat,
    _["work_conc"]      = ww_mat,
    _["community_conc"] = cc_mat,
    _["incidence"]      = incidence_mat,
    _["prevalence"]     = prevalence_mat,
    _["incidence_location"]      = incidence_location
  );

}
