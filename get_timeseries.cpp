#include <Rcpp.h>
#include <array>
#include <vector>
#include <random>
#include <string>

using namespace Rcpp;


// [[Rcpp::export]]
double hourly_r(int decay_days) {

  // first, calculate the annual r
  // from y = a * (1- r) ^t
  // if you are looking for y/a = 0.01
  // then R = 1 - (0.01)^(1/t)
  double r = 1 - std::pow(0.01, 1 / (double) decay_days);

  // next convert to hourly
  // hourly = (1 + daily)^(1/24) - 1
  double r2 = std::pow(1 + r, 1.0/24.0) - 1.0;

  return(r2);

}


// ************************************************************************* //
struct Person {

  // ---------------------
  // ARGUMENTS PROVIDED DURING CONSTRUCTION
  // these need to be in order in the constructor below
  // ---------------------
  // Personal identifies
  int id;
  int age;

  // IDS
  int household_id;
  int work_id;
  int school_id;
  int community_id;

  // Related to infectiousness
  bool asymptomatic;
  bool stays_home;
  const std::vector<int> baseline_time_activity;

  // a function of age, assigned at initialization
  double transmission_probability;
  // TODO: Eventually you'll want this to be for multiple
  // diseases right ..

  // and then various timesteps
  int incubation_days;
  int recovery_days;
  int waning_recovery_days;

  // ---------------------
  // INTERNAL HELPERS
  // ---------------------
  int SEIR_status = 0;
  //     0 = Susceptible
  //     1 = Exposed
  //     2 = Infected
  //     3 = Recovered

  // Clinical severity state
  int clinical_state = 0;
  //     0 = None (not currently infected)
  //     1 = I (subclinical infection)
  //     2 = U (URTI)
  //     3 = L (LRTI)
  //     4 = H (SLRTI)
  //     5 = R (Recovered)
  //     6 = D (Dead)

  // Scheduled clinical transition
  int next_clinical_state = 0;
  int transition_counter = 0;

  // Immunization and infection history
  int immunization_status = 0;
  int prior_infection = 0;

  bool is_incubating          = false;
  bool is_incident            = false;
  int location                = 0;
  int incidence_location      = -1;

  int timesteps_for_incubation = 24 * (double) incubation_days;
  int timesteps_for_recovery  = 24 * (double) recovery_days;
  int timesteps_for_waning_recovery = 24 * (double) waning_recovery_days;

  int incubation_counter      = timesteps_for_incubation;
  int recovery_counter        = timesteps_for_recovery;
  int waning_recovery_counter = timesteps_for_waning_recovery;

  bool ever_infected = false;

  // ---------------------
  // METHODS
  // ---------------------

  // *********************
  //  ** time-activity ***
  // *********************
  void set_location(int hour) {

    // Locations are:
    // 0 : Home
    // 1 : Work
    // 2 : School
    // 3 : Community

    // if infected and symptomatic,
    // theres an X% chance you stay at home
    if(SEIR_status == 2 & asymptomatic == false) {
      if(stays_home == true) {
        location = 0;
      } else {
        // otherwise youare infected but you still do your normal day
        location = baseline_time_activity[hour];
      }

    } else {
      // otherwise do your normal day
      location = baseline_time_activity[hour];
    }
  }

  // *************************************************
  // *** method to check if the person is infected ***
  // *************************************************
  void update_state(double p_people_infected) {

    // incident is only true if you turn incident this timestep
    is_incident = false;

    // -- SUSCEPTIBLE --
    if(SEIR_status == 0) {
      // if you are susceptible (SEIR = 0) and in an
      // infected zone, you are now exposed
      if(p_people_infected > 0.0) {
        SEIR_status = 1;
      }
    }

    // -- EXPOSED --
    if(SEIR_status == 1) {

      // if you are in an exposed zoned and
      // if you have't started to incubate yet
      // check if you start against your transmission probability
      // -- you are comparing your transmission probability
      //    against the percent of the population in the room that
      //    is infected. so its sort of like there are two things going on
      // (1) you have a 25% chance of contacting someone who is infected
      // (2) once you do contact someone, you have a 5% chance of getting it
      // multiply them together and compare that, 1x per hour.
      if(p_people_infected > 0.0) {
        if(is_incubating == false) {
          double xrand = runif(1, 0, 1)[0];
          if(xrand < (transmission_probability * p_people_infected)) {
            // infect();
            is_incubating = true;
            SEIR_status = 2;
            // TODO: need to change these to be .push()
            // because we will be running this over time
            is_incident = true;
            incidence_location = location;
            // and reset this so its ready for next time
            ever_infected = true;
          }
        }
      }

      // return to start the next timestep
      // maybe anytime after a timestep change you do a return?
      // that seems to make sense
      return;

    }

    // -- INFECTED --
    // Infected, now you are infected
    if(SEIR_status == 2) {

      // this is really the countdown to being symptomatic
      // but you can still infect people so these are just
      // components of being infected
      if(is_incubating) {
        incubation_counter--;
        if(incubation_counter == 0) {
          is_incubating = false;
          incubation_counter = timesteps_for_incubation;
          // *** TODO: insert something about symptom onset is now,
          // maybe this is where you get tested
        }
      } else {
        // once you are done incubating you can begin recovering
        // infected -> recovered
        recovery_counter--;
        if(recovery_counter == 0) {
          SEIR_status = 3;
          // and reset this so its ready for next time
          recovery_counter = timesteps_for_recovery;
        }
      }

      // you changed a counter, so return
      return;
    }

    // -- RECOVERED ---
    if(SEIR_status == 3) {
      waning_recovery_counter--;
      if(waning_recovery_counter == 0) {
        SEIR_status = 0; // you revert back to being susceptible
        // and reset this so its ready for next time
        waning_recovery_counter = timesteps_for_waning_recovery;
      }
      return;
    }

  }

  // *************
  // ** infect ***
  // *************
  void infect() {
    is_incubating = true;
    SEIR_status = 2;
    // TODO: need to change these to be .push()
    // because we will be running this over time
    is_incident = true;
    incidence_location = location;
    // and reset this so its ready for next time
    ever_infected = true;

    // initialize clinical infection process
    clinical_state = 1; // I: subclinical infection
    // all infected start with subclinical infection

    // call function to select next clinical state
    select_clinical_transition();

    // call function to select time before transition to next clinical state
    select_transition_duration();
  }

  // ********************************************************
  // *** select next clinical state (already infected)    ***
  // ********************************************************
  void select_clinical_transition() {
    // random draw from uniform distribution
    double transition_draw = R::runif(0.0, 1.0);

    // I: subclinical infection
    if (clinical_state == 1){
      // placeholder probabilities of transition
      // will eventually retrieve these based on
      // age, immunization status, prior infection hist
      double p_IU = 0.70;
      double p_IR = 0.29;

      if (transition_draw < p_IU){
        next_clinical_state = 2; // U: URTI
      }

      else if (transition_draw < p_IU + p_IR){
        next_clinical_state = 5; // R: Recovered
      }

      else {
        next_clinical_state = 6; // D: Dead
      }
    }

    // U: URTI
    else if (clinical_state == 2){
      double p_UL = 0.15;
      double p_UR = 0.84;

      if (transition_draw < p_UL){
        next_clinical_state = 3; // L: LRTI
      }

      else if (transition_draw < p_UL + p_UR){
        next_clinical_state = 5; // R: Recovered
      }

      else {
        next_clinical_state = 6; // D: Dead
      }
    }

    // L: LRTI
    else if (clinical_state == 3){
      double p_LH = 0.25;
      double p_LR = 0.72;

      if (transition_draw < p_LH){
        next_clinical_state = 4; // H: SLRTI
      }

      else if (transition_draw < p_LH + p_LR){
        next_clinical_state = 5; // R: Recovered
      }

      else {
        next_clinical_state = 6; // D: Dead
      }
    }

    // H: SLRTI
    else if (clinical_state == 4){
      double p_HR = 0.50;

      if (transition_draw < p_HR){
        next_clinical_state = 5; // R: Recovered
      }

      else {
        next_clinical_state = 6; // D: Dead
      }
    }
  }

  // *******************************************************************
  // *** select duration before transition to next clinical state    ***
  // *******************************************************************
  void select_transition_duration(){

    // I --> U
    // check agent's current clinical state and selected next state
    if (clinical_state == 1 && next_clinical_state == 2){
      transition_counter = 48; // Placeholder duration of 48 hours
      // later replace 48 with sampled duration from appropriate vector
    }

    // I --> R
    if (clinical_state == 1 && next_clinical_state == 5){
      transition_counter = 120; // Placeholder: 120 hours (5 days)
    }

    // I --> D
    if (clinical_state == 1 && next_clinical_state == 6){
      transition_counter = 120; // Placeholder: 120 hours (5 days)
    }

    // U --> L
    if (clinical_state == 2 && next_clinical_state == 3){
      transition_counter = 48;
    }

    // U --> R
    if (clinical_state == 2 && next_clinical_state == 5){
      transition_counter = 120;
    }

    // U --> D
    if (clinical_state == 2 && next_clinical_state == 6){
      transition_counter = 120;
    }

    // L --> H
    if (clinical_state == 3 && next_clinical_state == 4){
      transition_counter = 48;
    }

    // L --> R
    if (clinical_state == 3 && next_clinical_state == 5){
      transition_counter = 120;
    }

    // L --> D
    if (clinical_state == 3 && next_clinical_state == 6){
      transition_counter = 120;
    }

    // H --> R
    if (clinical_state == 4 && next_clinical_state == 5){
      transition_counter = 120;
    }

    // H --> D
    if (clinical_state == 4 && next_clinical_state == 6){
      transition_counter = 120;
    }
  }

  // ****************************************************
  // *** update clinical state as duration elapses    ***
  // ****************************************************
  void update_clinical_state(){

    // decrease transition_counter hourly
    if (transition_counter > 0){
      transition_counter--;
    }

    // move agent to next clinical state when duration elapses
    if (transition_counter == 0 && clinical_state != 0
          && clinical_state != 5 && clinical_state != 6){
      // prevent uninfected, recovered, dead agents
      // from transitioning to a new clinical state
      clinical_state = next_clinical_state;

      // select next transition upon entry into next clinical state
      // if still infected
      if(clinical_state >= 1 && clinical_state <= 4){
        select_clinical_transition();
        select_transition_duration();
      }
    }
  }

  // *************
  // ** print  ***
  // *************
  void print_self() {
    Rcpp::Rcout << "Person ID# "<< id << ", ";
    Rcpp::Rcout << "SEIR_status: " << SEIR_status << "\n";
  }

  // *********************************************
  // constructor -- this ensures that when you
  // create the object it has valid properties
  // *********************************************
  Person(int id_,
         int age_,
         int hh_,
         int work_,
         int school_,
         int community_,
         bool asymptomatic_,
         bool stays_home_,
         const std::vector<int> baseline_time_activity_,
         double transmission_probability_,
         int incubation_days_,
         int recovery_days_,
         int waning_recovery_days_)
    : id(id_),
      age(age_),
      household_id(hh_),
      work_id(work_),
      school_id(school_),
      community_id(community_),
      asymptomatic(asymptomatic_),
      stays_home(stays_home_),
      baseline_time_activity(baseline_time_activity_),
      transmission_probability(transmission_probability_),
      incubation_days(incubation_days_),
      recovery_days(recovery_days_),
      waning_recovery_days(waning_recovery_days_){}
};

// **************************************************************************//
struct MicroEnvironment {

  // ---------------------
  // ARGUMENTS
  // ---------------------
  int id;
  int location;            // 0: Home, 1: Work, 2: School, 3: Community
  int decay_days; // how long can the virus last in the air

  // ---------------------
  // INTERNAL
  // ---------------------
  int hourly_decay_rate = hourly_r(decay_days);
  int timesteps_for_decay = 24 * decay_days;
  int decay_counter = timesteps_for_decay;
  //
  bool zone_is_infected = false;
  //
  std::vector<int> member_ids;
  int n_members = 0;
  double total_p_members_infected;
  int last_n_members_infected;

  // ---------------------
  // METHODS
  // ---------------------
  // add a person and update the size
  void add_person_id(int person_id) {
    member_ids.push_back(person_id);
    n_members++;
  };

  void print() {
    std::string loc_str;
    if(location == 0) loc_str = "Household";
    if(location == 1) loc_str = "Workplace";
    if(location == 2) loc_str = "School";
    if(location == 3) loc_str = "Community";

    Rcpp::Rcout << loc_str << " " << id << ": " << zone_is_infected << "\n";
  }

  // ************
  // ** update **
  // ************
  void update_state(
      int hour_i,
      std::vector<Person>& people // pass by reference so you aren't copying it
  ) {

    // Simplifying this
    // if more than R0 math % of people are in this place, its infected
    // right now you can do this in this function, but it could
    // make sense to do this in a struct

    // you should also do this in a struct because otherwise you are doing
    // the same lookup each time

    int person_id;
    int n_members_infected = 0;
    int non_local_infected_members = 0;
    double p_members_infected = 0.0;

    // define if this zone is presently infected
    for(int pi = 0; pi < n_members; pi++) {
      person_id = member_ids[pi];
      // first check that they are here at this time
      if(people[person_id].location == location) {
        // then if they are incubating or infected,
        // this adds to the percent of the
        // environment that is infected
        if(people[person_id].SEIR_status == 2) {
          n_members_infected++;
        }
      } else {
        if(people[person_id].SEIR_status == 2) {
          non_local_infected_members++;
        }
      }
    }

    // the overall size regardless of location
    total_p_members_infected =
      (double) (n_members_infected + non_local_infected_members) /
        (double) n_members;

    // Ok so there are two mechanisms for people to get infected
    // (1) from random chance of other people in the environment
    //     but this needs to be scaled so its a percentage of all
    //     people in the environment, so that houses have a similar
    //     number of draws as the community
    // (2) from virus in the air itself, this is only when the decay
    //     of the people from the last

    // So `n_people_infected` is handled above

    // --- Is the zone infected ---
    // If anyone is here is infected, consider it to be infected
    // this has minimal impacts because you are juse adding +1
    // to the draw against transmission probability
    // and it lingers
    if(n_members_infected > 0) {

      // update this
      zone_is_infected = true;
      last_n_members_infected = n_members_infected;

      // make a percentage
      // the reason you do this is so that households can compete
      // with larger networks in terms of potential for infection
      p_members_infected = (double) n_members_infected / (double) n_members;

    } else {

      // was it just infected and now its not? start the counter
      if(zone_is_infected) {
        //
        decay_counter--;

        // how many hours have gone by
        double hours_elapsed = (double) timesteps_for_decay -
          (double) decay_counter;

        // apply the exponential decay hourly to the last n members
        // the recent high water mark
        double proxy_n_infected = (last_n_members_infected) *
          std::pow(1 - hourly_decay_rate, hours_elapsed);

        // // make a percentage
        p_members_infected = (double) proxy_n_infected / (double) n_members;

        if(p_members_infected == 0.0) {
          Rcpp::Rcout << "n_members: " << n_members << "\n";
          Rcpp::Rcout << "n_members infected: " << n_members_infected << "\n";
          Rcpp::stop("P == 0\n");
        }

      }

      // if the room has been empty for long enough,
      // it becomes safe again
      if(decay_counter == 0) {
        zone_is_infected = false;
        decay_counter = timesteps_for_decay;
        p_members_infected = 0.0;
      }

    }

    // make some validity checks
    if(p_members_infected > 1.0) {
      Rcpp::Rcout << "n_members: " << n_members << "\n";
      Rcpp::Rcout << "n_members infected: " << n_members_infected << "\n";
      Rcpp::stop("P > 100\n");
    }

    // then update the status of each person
    for(int pi = 0; pi < n_members; pi++) {
      person_id = member_ids[pi];
      if(people[person_id].location == location) {
        people[person_id].update_state(p_members_infected);
      }
    }

  };

  // *********************************************
  // constructor -- this ensures that when you
  // create the object it has valid properties
  // *********************************************
  MicroEnvironment(
         int id_,
         int location_,
         int decay_days_)
    : id(id_),
      location(location_),
      decay_days(decay_days_) {}

};

// **************************************************************************//
// [[Rcpp::export]]
List get_timeseries(
    IntegerMatrix df,
    IntegerMatrix time_activity,
    int n_days,
    double transmission_probability,
    int virus_decay_days,
    int incubation_days,
    int recovery_days,
    IntegerVector personIDs_to_track,
    IntegerVector hhIDs_to_track,
    IntegerVector workIDs_to_track,
    IntegerVector schoolIDs_to_track,
    IntegerVector commIDs_to_track
) {

  // ----------------------------------------------
  // **** INITIALIZE CONSTANTS *****
  // ----------------------------------------------
  Rcpp::Rcout << "Initializing\n";

  const int n_hours_per_day = 24;
  const int n_timesteps = n_hours_per_day * n_days;
  const int n_people = df.nrow();

  const int n_ages = 100 + 1; //0 to 100

  // these are zero indexed, so add 1 to get max
  const int n_hh          = Rcpp::max(df(_, 2)) + 1;
  const int n_work        = Rcpp::max(df(_, 3)) + 1;
  const int n_schools     = Rcpp::max(df(_, 4)) + 1;
  const int n_communities = Rcpp::max(df(_, 5)) + 1;

  const int ntrack_people = personIDs_to_track.size();
  const int ntrack_hh = hhIDs_to_track.size();
  const int ntrack_ww = workIDs_to_track.size();
  const int ntrack_ss = schoolIDs_to_track.size();
  const int ntrack_cc = commIDs_to_track.size();

  // ----------------------------------------------
  // **** TRACKING MATRICES *****
  // ----------------------------------------------
  Rcpp::Rcout << "Creating tracking matrices\n";

  // get matrices for yes/no is infect in each environment and time
  // rearranging this so the largest dimension is the rows
  Rcpp::IntegerMatrix seir_mat(n_timesteps, ntrack_people);
  Rcpp::IntegerMatrix person_location_mat(n_timesteps, ntrack_people);
  Rcpp::NumericMatrix hh_mat(n_timesteps, ntrack_hh);
  Rcpp::NumericMatrix ww_mat(n_timesteps, ntrack_ww);
  Rcpp::NumericMatrix ss_mat(n_timesteps, ntrack_ss);
  Rcpp::NumericMatrix cc_mat(n_timesteps, ntrack_cc);

  // Also want to track age-specific incidence and prevalence
  // Incidence is number of *new cases* this timestep
  // Prevalence is total number of cases at this time
  Rcpp::IntegerMatrix incidence_mat(n_timesteps, n_ages);
  Rcpp::IntegerMatrix prevalence_mat(n_timesteps, n_ages);

  // and i want to track the proportion of the population that
  // were ever infected
  Rcpp::DoubleVector p_ever_infected(n_timesteps);

  // ----------------------------------------------
  // **** AGENT OBJECTS *****
  // ----------------------------------------------
  Rcpp::Rcout << "Creating agents\n";
  std::vector<Person> people;
  people.reserve(n_people);

  for (int i = 0; i < n_people; ++i) {

    // when you are sick are you asymptomatic
    bool asymptomatic = (bool) df(i, 6) == 1;

    // when you are sick do you stay home
    bool stays_home = (bool) df(i, 7) == 1;

    // pick a time-activity pattern
    std::vector<int> ta(24);

    for(int hi = 0; hi < 23; hi++) {
      if(df(i, 3) > -1) { // has a work id
        ta[hi] = time_activity(hi, 1);
      } else if(df(i, 4) > -1) { // use the school location
        ta[hi] = time_activity(hi, 2);
      } else { // use the community location
        ta[hi] = time_activity(hi, 3);
      }
    }

    // create the person object
    people.emplace_back(
      df(i, 0),     // ID
      df(i, 1),     // age
      df(i, 2),     // household_id
      df(i, 3),     // work_id
      df(i, 4),     // school_id
      df(i, 5),     // community_id
      asymptomatic, // asymptomatic
      stays_home,   // stays_home
      ta,           // time-activity matrix
      transmission_probability,          // transmission probability
      incubation_days,         // timesteps for incubation
      recovery_days,         // timesteps for recovery
      365        // timesteps for waning recovery
    );
  }

  // ----------------------------------------------
  // **** NETWORK LOOKUP TABLES *****
  // ----------------------------------------------
  // build the lookup tables
  Rcpp::Rcout << "Environment objects\n";

  // lists of Environments
  std::vector<MicroEnvironment> households;
  std::vector<MicroEnvironment> workplaces;
  std::vector<MicroEnvironment> schools;
  std::vector<MicroEnvironment> communities;

  // Make each blank one
  // location = 0: Home, 1: Work, 2: School, 3: Community
  for(int i = 0; i < n_hh; i++) {
    households.emplace_back(
      i,    // id
      0,    // location
      virus_decay_days // how long can the virus last in the air
    );
  }

  for(int i = 0; i < n_work; i++) {
    workplaces.emplace_back(
      i,    // id
      1,    // location
      virus_decay_days // how long can the virus last in the air
    );
  }

  for(int i = 0; i < n_schools; i++) {
    schools.emplace_back(
      i,    // id
      2,    // location
      virus_decay_days // how long can the virus last in the air
    );
  }

  for(int i = 0; i < n_communities; i++) {
    communities.emplace_back(
      i,    // id
      3,    // location
      virus_decay_days // how long can the virus last in the air
    );
  }

  // now add people
  // only add if the ID is greater than 0
  Rcpp::Rcout << "Add agents to environments\n";
  // int i = 0;
  // if(df(i, 2) >= 0) households[df(i, 2)].add_person_id(i);
  for (int i = 0; i < n_people; ++i) {
    if(df(i, 2) >= 0) households[df(i, 2)].add_person_id(i);
    if(df(i, 3) >= 0) workplaces[df(i, 3)].add_person_id(i);
    if(df(i, 4) >= 0) schools[df(i, 4)].add_person_id(i);
    if(df(i, 5) >= 0) communities[df(i, 5)].add_person_id(i);
  }

  // ----------------------------------------------
  // **** Seed the infections *****
  // ----------------------------------------------

  //TODO: Clarify how initially seeded infections should be counted
  //in cumulative infection outcomes.
  //Currently, seeded agents are assigned SEIR_status=2 but
  //ever_infected=false, so seeds are excluded from p_ever_infected
  //unless they become reinfected.
  //Consider whether to track:
  //(1) proportion ever infected, including initial seeds
  //(2) proportion newly infected during the simulation, exlcuding seeds

  // seed the infectors
  Rcpp::Rcout << "Initial infectors\n";
  for(int pi = 0; pi < n_people; pi++) {
    if(df(pi, 8) == 1) {
      people[pi].SEIR_status = 2;
    }
  }

  // ----------------------------------------------
  // **** MAIN LOOP *****
  // ----------------------------------------------
  Rcpp::Rcout << "Main loop\n";

  // overall timestep counter
  int timestep = 0;

  // helpers
  int this_id;

  // incidence and prevalence
  Rcpp::IntegerVector incidence_sum(n_ages);
  Rcpp::IntegerVector prevalence_sum(n_ages);

  // incidence locations
  Rcpp::IntegerMatrix incidence_location(n_ages, 4);

  Rcpp::Rcout << "********************************"<< "\n";
  Rcpp::Rcout << "DAY: ";

  // loop
  for(int day_i = 0; day_i < n_days; day_i++) {

    if(day_i % 10 == 0) Rcpp::Rcout << day_i << "\t";

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
      for(int hh_i = 0; hh_i < n_hh; hh_i++) {
        households[hh_i].update_state(hour_i, people);
      }

      // ALL WORK
      for(int ww_i = 0; ww_i < n_work; ww_i++) {
        workplaces[ww_i].update_state(hour_i, people);
      }

      // ALL SCHOOL
      for(int ss_i = 0; ss_i < n_schools; ss_i++) {
        schools[ss_i].update_state(hour_i, people);
      }

      // ALL COMMUNITY
      for(int cc_i = 0; cc_i < n_communities; cc_i++) {
        communities[cc_i].update_state(hour_i, people);
      }

      // ***********************
      // UPDATE CLINICAL SEVERITY PROGRESSION
      for(int pi = 0; pi < n_people; pi++){
        people[pi].update_clinical_state();
      }

      // ***********************
      // now save the ones you are tracking
      // assumes you have one in each
      for(int ti = 0; ti < ntrack_people; ti ++) {
        this_id = personIDs_to_track[ti];
        seir_mat(timestep, ti) = people[this_id].SEIR_status;
        person_location_mat(timestep, ti) = people[this_id].location;
      }
      for(int ti = 0; ti < ntrack_hh; ti ++) {
        this_id = hhIDs_to_track[ti];
        hh_mat(timestep, ti) = households[this_id].total_p_members_infected;
      }
      for(int ti = 0; ti < ntrack_ww; ti ++) {
        this_id = workIDs_to_track[ti];
        ww_mat(timestep, ti) = workplaces[this_id].total_p_members_infected;
      }
      for(int ti = 0; ti < ntrack_ss; ti ++) {
        this_id = schoolIDs_to_track[ti];
        ss_mat(timestep, ti) = schools[this_id].total_p_members_infected;
      }
      for(int ti = 0; ti < ntrack_cc; ti ++) {
        this_id = commIDs_to_track[ti];
        cc_mat(timestep, ti) = communities[this_id].total_p_members_infected;
      }

      // ***********************
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

      // ***********************
      // and the percent of people ever infected
      int total_ever_infected = 0;
      for(int pi = 0; pi < n_people; pi++) {
        total_ever_infected += (int) people[pi].ever_infected;
      }
      p_ever_infected[timestep] = (double) total_ever_infected /
        (double) n_people;

      // ***********************
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
    _["person_seir"]     = seir_mat,
    _["person_location"] = person_location_mat,
    _["household_conc"]  = hh_mat,
    _["school_conc"]     = ss_mat,
    _["work_conc"]       = ww_mat,
    _["community_conc"]  = cc_mat,
    _["incidence"]       = incidence_mat,
    _["prevalence"]      = prevalence_mat,
    _["incidence_location"]      = incidence_location,
    _["p_ever_infected"] = p_ever_infected
  );

}

// **************************************************************************//
// function to test clinical progression components of model on individual agent
// [[Rcpp::export]]
Rcpp::IntegerVector test_clinical_progression_indiv(int n_hours = 500) {
  //returns clinical_state codes at each simulated hour

  //create test agent
  Person person(
    0,                      //id
    25,                     //age
    0,                      //household_id (required by constructor)
    -999,                   //work_id (no assigned workplace)
    -999,                   //school_id (no assigned school)
    0,                      //community_id (required by constructor)
    false,                  //asymptomatic (required by constructor, not used by clinical progression)
    false,                  //stays_home (required by constructor, not used by clinical progression)
    std::vector<int>(24,0), //baseline_time_activity (all 24 hours at home)
    0.00125,                //transmission_probability (required by constructor, not used by clinical progression)
    3,                      //incubation_days (required by constructor, not used by clinical progression)
    3,                      //recovery_days (required by constructor, not used by clinical progression)
    365                     //waning_recovery_days (required by constructor, not used by clinical progression)
  );

  //call infect() to initialize infection and clinical progression in test person
  person.infect();
    //initializes SEIR infection vars (is_incubating, SEIR status, is_incident, incidence_location, ever_infected)
    //agent enters clinical state I
    //next clinical state selected
    //duration until first transition is assigned

    //hourly loop below advanced clinical progression until terminal state reached (recovery or death)
    //or until simulation ends

  //store clinical_state at each hour
  Rcpp::IntegerVector clinical_states(n_hours + 1);

  //record initial clinical_state at hour 0
  clinical_states[0] = person.clinical_state;
    //should be clinical_states[0] = 1 because clinical_state set to 1 in person.infect()

  //create loop to simulate advancing clinical progression hourly (after first hour)
  for (int hour = 1; hour <= n_hours; hour++){
    //update clinical progression (method in person structure)
    person.update_clinical_state();
    //record clinical state after hourly update
    clinical_states[hour] = person.clinical_state;
  }

  //return vector clinical_states, which tracks progression in clinical_state hourly
  return clinical_states;
}

// **************************************************************************//
// function to test clinical progression components of model on pop of 10000 agents
// [[Rcpp::export]]
Rcpp::IntegerMatrix test_clinical_progression_pop(int n_agents = 10000, int n_hours = 500) {

  //create matrix to store clinical states for all agents each hour
  Rcpp::IntegerMatrix clinical_states(n_hours + 1, n_agents);

  //create vector to store test agents
  std::vector<Person> people;

  //reserve memory for n_agents
  people.reserve(n_agents);

  //create population of test agents and initialize clinical progression
  //loop runs once per agent
  for (int i=0; i < n_agents; i++){
    //create new agents using emplace_back()
    people.emplace_back(
      i,                      //id
      25,                     //age
      0,                      //household_id
      -999,                   //work_id
      -999,                   //school_id
      0,                      //community_id
      false,                  //asymptomatic
      false,                  //stays_home
      std::vector<int>(24,0), //baseline_time_activity
      0.00125,                //transmission_probability
      3,                      //incubation_days
      3,                      //recovery_days
      365                     //waning_recovery_days
    );

    //initialize infection and clinical progression for each agent using infect()
    people[i].infect();

    //record initial clinical state for each agent at hour 0 and record in matrix
    clinical_states(0, i) = people[i].clinical_state;
  }

  //simulate clinical progression for all agents

  //advance clinical progression one hour at a time
  for (int hour=1; hour <= n_hours; hour++){
    //update each agent during the current hour
    for (int i=0; i<n_agents; i++){
      //update clinical progression
      people[i].update_clinical_state();

      //record clinical state after hourly update and record in matrix
      clinical_states(hour, i) = people[i].clinical_state;
    }
  }

  //return matrix of clinical trajectories for all agents in population
  return clinical_states;
}



