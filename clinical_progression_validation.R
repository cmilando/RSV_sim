# =============================================================================
#RSV CLINICAL PROGRESSION VALIDATION AND DIAGNOSTICS
#Author: Hannah Thomas
#Last Updated: 11/10/26
# =============================================================================

#Purpose:
#validate the clinical progression components of the RSV ABM
#using placeholder transition probabilities and fixed transition
#durations.


#This Script...
#compiles the C++ simulation code using Rcpp
#defines expected transition probabilities and durations
#tests clinical progression for a single agent
#tests clinical progression for a population of 10000 agents
#compares simulated transition probabilities and durations with expected values
#produces diagnostic plots of clinical trajectories, occupancy of
#clinical states, and transition probabilities

#NOTE: This code DOES NOT run the full ABM or require demo_pop.RDS
#The tests are conducted independently of environmental transmission
#and the existing recovery process in the SEIR model.

# =============================================================================
#SETUP
# =============================================================================

library(Rcpp)
library(ggplot2)
library(tidyr)
library(gt)

# =============================================================================
# COMPILE C++ CODE
# =============================================================================

#compile get_timeseries.cpp and expose function marked with [[Rcpp:export]]
#so they can be called in R directly
Rcpp::sourceCpp("get_timeseries.cpp")

#verify that both clinical progression test functions are present
#error if either condition is false
stopifnot(
  exists("test_clinical_progression_indiv", mode = "function"),
  exists("test_clinical_progression_pop", mode = "function")
  )

# =============================================================================
# DEFINE EXPECTED TRANSITION PROBABILITIES AND DURATIONS
# =============================================================================

#define expected values from select_clinical_transition() and
#select_transition_duration() in get_timeseries.cpp

#serve as reference values for validating the simulated outputs

#NOTE: need to update this table if placeholder parameters in C++ change

#clinical states represented by integers in C++:
#0 = none
#1 = I (subclinical infection)
#2 = U (URTI)
#3 = L (LRTI)
#4 = H (SLRTI)
#5 = R (recovered)
#6 = D (dead)
states <- c("None", "I", "U", "L", "H", "R", "D")

#function to convert numeric clinical state codes to readable labels
#add 1 bc C++ state codes begin at 0, but R vectors are indexed starting at 1
#ex. state_name(1) returns "I" and state_name(5) returns "R"
state_name <- function(x) states[x + 1L]

#create reference table containing all clinical transitions
#from: clinical state agent is leaving
#to: clinical state agent is entering
#probability: conditional probability of selecting that destination given current state
#duration_hours: fixed time until selected transition occurs (destination-specific durations)
expected <- data.frame(
  from = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L),
  to = c(2L, 5L, 6L, 3L, 5L, 6L, 4L, 5L, 6L, 5L, 6L),
  probability = c(0.70, 0.29, 0.01, 0.15, 0.84, 0.01, 0.25, 0.72, 0.03, 0.50, 0.50),
  duration_hours = c(48L, 120L, 120L, 48L, 120L, 120L, 48L, 120L, 120L, 120L, 120L)
)

#create readable transition labels (e.g. "I->U") to match observed w/ expected
expected$transition <- paste0(state_name(expected$from), "->", state_name(expected$to))

# =============================================================================
# FUNCTION TO EXTRACT COMPLETED CLINICAL TRANSITIONS
# =============================================================================

#C++ test functions (test_clinical_progression_indiv and test_clinical_progression_pop)
#return trajectories of clinical states for agents
#rows = simulated hours, hour 0 through n_hours
#columns = individual agents, agent 1 ... n_agents

#want to identify when each agent changes clinical state, which transition occurred,
#and how long the agent spend in the originating state before that transition

#this function converts the clinical state matrix into a data frame containing one row
#per completed transition

get_transitions <- function(mat){
  #verify that the input is a matric with at least 2 rows b/c at least 2 observations
  #are required to detect a change in clinical state
  stopifnot(is.matrix(mat), nrow(mat) >= 2L)

  #create two matrices containing consecutive clinical states
  #previous excludes the last row (hours 0 through n_hours - 1)
  #current excludes the first row (hours 1 through n_hours)
  #matrices contain same agents and dimensions, shifted by one simulation hour
  previous <- mat[-nrow(mat), , drop = FALSE]
  current <- mat[-1L, , drop = FALSE]

  #identify positions where clinical state changed
  #current != previous returns TRUE any place where an agent's clinical state differs
  #from the previous hour
  #which(..., arr.ind=TRUE) returns the row and column indices of these changes
  #row index = hour of completed transition
  #column index = agent to whom changes occurred
  idx <- which(current != previous, arr.ind = TRUE)

  #if no transitions occurred, return an empty data frame with the expected columns
  if(nrow(idx) == 0L){
    return(data.frame(agent = integer(),
                      hour = integer(),
                      from = integer(),
                      to = integer(),
                      duration_hours = integer()))
  }

  #construct a data frame containing completed transitions
  #for each detected change, agent ids the individual, hour ids when the transition
  #occurred, from ids the clinical state before transition, to ids the clinical state
  #after transition
  events <- data.frame(
    agent = idx[, 2L],
    hour = idx[, 1L],
    from = previous[idx],
    to = current[idx]
  )

  #sort transitions first by agent, then chronologically
  #allows us to calculate time between successive transitions for each agent
  events <- events[order(events$agent, events$hour),]
  rownames(events) <- NULL

  #determine when each agent entered the state they are leaving
  #every test agent begins in state I at hour 0
  #for the first transition, state entry is hour 0
  #for later transitions, entry time is the hour of that agent's previous transition
  #ave() applies the calculation separately within each agent
  #head(x, -1L) retrieved all but the last transition hour
  previous_hour <- ave(events$hour, events$agent, FUN = function(x) c(0L, head(x, -1L)))

  #calculate the observed time spent in each originating state
  #b/c each agent's transition counter initializes upon state entry and decreases hourly,
  #difference between successive transition times should equal assigned transition-specific
  #duration
  events$duration_hours <- events$hour - previous_hour

  #return the record of completed transitions
  events
}

# =============================================================================
# SINGLE-AGENT CLINICAL PROGRESSION VALIDATION
# =============================================================================

#set seed for reproducibility
set.seed(99)

#simulate clinical progression for one agent over 500 hours
#agent begins in I, selects a destination and duration, and progresses hourly
#until reaching recovery or death
#returned vector contains clinical state at hours 0 through n_hours
test_indiv <- test_clinical_progression_indiv(n_hours = 500L)

#convert vector to one-column matrix so we can pass it to get_transitions() (see above)
indiv_mat <- matrix(test_indiv, ncol = 1L)

#extract completed clinical transitions and observed durations
indiv_events <- get_transitions(indiv_mat)

#create readable transition labels for each completed transition
indiv_events$transition <- paste0(state_name(indiv_events$from), "->", state_name(indiv_events$to))

#retrieve expected duration for each observed transition
#match() finds position of each observed transition label in expected reference table
#use positions to retreieve corresponding expected transition durations
indiv_events$expected_duration <- expected$duration_hours[match(indiv_events$transition, expected$transition)]

#compare observed and expected durations
#TRUE indicates transition occurred after assigned number of hours
indiv_events$duration_correct <- with(indiv_events, duration_hours == expected_duration)

#print validation results for individual agent clinical progression
cat("\n=== INDIVIDUAL AGENT TEST ===\n")

print(indiv_events[, c("hour", "transition", "duration_hours", "expected_duration", "duration_correct")], rownames = FALSE)

cat("Initial State:", state_name(test_indiv[1L]), "\n")
cat("Final State:", state_name(tail(test_indiv, 1L)), "\n")

#perform validation checks
#stop if any check fails
stopifnot(test_indiv[1L] == 1L, #check that agent begins in I
          tail(test_indiv, 1L) %in% c(5L,6L), #check that agent reaches a terminal clinical state (R or D)
          all(!is.na(indiv_events$expected_duration)), #check that every completed transition is included in reference table
          all(indiv_events$duration_correct)) #check that every observed transition duration matches its expected duration

# =============================================================================
# SINGLE-AGENT DIAGNOSTIC PLOT
# =============================================================================

#convert vector of clinical states to a data frame
#each row is one observation
#hour = simulated hour (0-500)
#clinical_state = the agent's clinical state at the specified hour
indiv_df <- data.frame(hour = 0:(length(test_indiv) - 1L), clinical_state = test_indiv)

#plot clinical state as f(time)
#geom_step because state changes occur at discrete times
#state labels replace numeric codes on y-axis
p_indiv <- ggplot(indiv_df, aes(hour, clinical_state)) +
  geom_step(linewidth = 0.8) +
  scale_y_continuous(breaks = 1:6,
                     labels = c("I", "U", "L", "H", "R", "D"),
                     limits = c(0.8, 6.2),
                     minor_breaks = NULL
                     ) +
  labs(title = "Individual Agent Clinical Trajectory",
       x = "Hours Since Infection",
       y = "Clinical State") +
  theme_classic()

#display plot
p_indiv

# =============================================================================
# POPULATION-LEVEL CLINICAL PROGRESSION VALIDATION
# =============================================================================

#######Set Up#######

#Moving to population of agents means you can evaluate all transition types across
#many agents

#set seed for reproducibility
set.seed(123)

#simulate clinical progression for 10,000 agents over 500 hours
#returns a matrix instead of a vector
#rows represent simulation hours (0 through 500)
#columns represent individual agents (1 through 10,000)
#all agents begin in I at hour 0 and independently select subsequent clinical
#transitions using placeholder probabilities defined in get_timeseries.cpp
test_pop <- test_clinical_progression_pop(n_agents = 10000L, n_hours=500L)

#extract completed transitions for every agent
#processes all 10,000 trajectories simultaneously
#each row represents one completed transition NOT ONE AGENT
#an agent can contribute multiple observations depending on its clinical trajectory
#ex. an agent with trajectory I -> U -> R produces 2 observations and an agent with
#trajectory I -> R produces one observation and an agent with trajectory
#I -> U -> L -> H -> D produces four observations
#output includes agent ID, transition hour, originating state, destination state,
#and observed duration in the originating state before transition
pop_events <- get_transitions(test_pop)

#add readable labels for each transition
#labels used to match simulated transitions to expected probabilities and durations
#defined above
pop_events$transition <- paste0(state_name(pop_events$from), "->", state_name(pop_events$to))

#check first few completed transitions
#can see successive transitions for each agent in chronological order
head(pop_events)

#######Validate Transition Durations#######

#retrieve expected duration for each observed transition from the reference table
#defined above
#lookup for ALL completed transitions across population, instead of all transitions
#for one agent
pop_events$expected_duration <- expected$duration_hours[match(pop_events$transition, expected$transition)]

#compare observed and expected durations
#currently, durations are fixed and depend on the transition
#every completed transition with same origin and destination state should have the
#same duration
#TRUE indicates transition occurred at the assigned time (specified by duration placeholder
#in function select_transition_duration() coded within get_timeseries.cpp)
pop_events$duration_correct <- with(pop_events, duration_hours == expected_duration)

#count transitions with correct, incorrect or missing expected durations
#evaluates every completed transition in the population
#total number fo records can (and probably will) be greater than number of agents
#expected results: all TRUE with no FALSE or NA values
table(pop_events$duration_correct, useNA = "ifany")

#verify all completed transitions are allowed and all observed durations match expected
#values
#if all works correctly should show only TRUE and execute without error
stopifnot(
  all(test_pop[1, ] == 1L), # check that agent begings in I at hour 0
  all(test_pop[nrow(test_pop), ] %in% c(5L, 6L)), #check that agent reaches either recovery or death by hour 500
  all(!is.na(pop_events$expected_duration)), #check that every completed transition is included in reference table
  all(pop_events$duration_correct) #check every observed transition duration equals assigned duration for the transition
)

#######Summarize Transition Durations by Type#######

#summarize durations separately for each transition type
#shows how often each transition occurs and whether observed durations varies
#within transition types
#aggregated() divides the transition records into groups based on transition labels
#for each group, function calculates number of completed transitions of that type,
#minimum observed transition duration
#maximum observed transition duration
#b/c model currently uses fixed durations, min and max should be equal to one another
#for each transition type and equal to the expected duration that corresponds to the
#specified transition type
duration_summary <- aggregate(
  duration_hours ~ transition,
  data = pop_events,
  FUN = function(x) c(
    n = length(x),
    min = min(x),
    max = max(x)
  )
)

#display duration summary for all observed transition types
duration_summary

#######Validate Conditional Transition Probabilities#######

#count completed transitions for each combination of originating and destination states
#b/c for probability validation, need to calculate how frequently each transition occurs
#aggregated groups by 'from' and 'to'
#length() counts the number of completed transitions in each group
transition_counts <- aggregate(
  agent ~ from + to,
  data = pop_events,
  FUN = length
)

#rename count column
names(transition_counts)[3] <- "n"

#review observed transition counts
transition_counts

#Calculate total number of completed transitions out of each originating clinical state
#this is denominator for conditional transition probabilities
#ex. all agents entering U eventually transition to L, R, or D,
#so N_exiting_U = N(U->L) + N(U->R) + N(U->D)
#with current test conditions (n_hours=500), there is enough time for every agent to reach
#a terminal clinical state (R or D) before simulation ends
#so number of agents completing transitions out of each state = number of agents entering each state
state_totals <- aggregate(
  n ~ from,
  data = transition_counts,
  FUN = sum
)

#rename denom column
names(state_totals)[2] <- "n_from"

#review observed agents exiting each originating state
state_totals

#calculate observed conditional probabilities
#add total number of exits from each originating state to transition-specific counts
#merge() matches rows based on 'from' variable
#each transition from the same originating state received same denominator
transition_counts <- merge(
  transition_counts,
  state_totals,
  by = "from"
)

#calculate observed probabilities
#numerator = number of completed transitions from state j to state k
#denominator = total number of completed transitions out of state j
#estimates p(selecting destination state k|entering state j)
transition_counts$observed_probability <- transition_counts$n / transition_counts$n_from

#add readable labels
transition_counts$transition <- paste0(state_name(transition_counts$from), "->", state_name(transition_counts$to))

#print probabilities
transition_counts

#compare observed and expected probabilities
#match observed transitions to expected probabilities
#match() ids corresponding transition in the expected reference table (defined above)
transition_counts$expected_probability <- expected$probability[match(transition_counts$transition, expected$transition)]

#calculate difference between observed and expected probabilities
#positive values indicate a transition occurred more frequently than expected
#negative values indicate a transition occurred less frequently than expected
#expect small differences because this part of simulation is stochastic
#observed and expected probabilities DO NOT HAVE TO BE EQUAL
transition_counts$difference <- transition_counts$observed_probability - transition_counts$expected_probability

#create comparison table
#organize columns
probability_summary <- transition_counts[
  order(transition_counts$from, transition_counts$to),
  c("transition",
    "n",
    "n_from",
    "observed_probability",
    "expected_probability",
    "difference")
]

#print results w/ 4 decimal places
print(probability_summary, digits = 4, row.names = FALSE)

#verify that observed transition probabilities sum to 1 for each originating clinical state
#every agent entering a state should eventuall transition to one of the allowed destinations
probability_sums <- aggregate(
  observed_probability ~ from,
  data = transition_counts,
  FUN = sum
)

#check with some wiggle room
stopifnot(
  all(abs(probability_sums$observed_probability - 1) < 1e-10)
)

probability_sums

#######Validate Cumulative Clinical Severity Outcomes#######

#compare proportion of all simulated infections that reach each clinical severity
#state with expected probabilities (from specified conditional transition probabilities)
#uses all 10,000 initially infected agents as denom

#define expected cumulative probabilities from conditional probabilities
#from get_timeseries.cpp
expected_cumulative <- c(
  U = 0.70,
  L = 0.70 * 0.15,
  H = 0.70 * 0.15 * 0.25,
  D = 0.01 +
    0.70 * 0.01 +
    0.70 * 0.15 * 0.03 +
    0.70 * 0.15 * 0.25 * 0.50
)

#for each agent, determine whether they ever entered U, L, or H
#apply(test_pop,...) evaluates each agent independently
#any(x ==2L) returns TRUE if an agent observed at U at least once in simulation
#all agents followed until terminal states (R and D),
#so able to identify cumulative clinical outcomes
ever_U <- apply(test_pop, 2, function(x) any(x == 2L))
ever_L <- apply(test_pop, 2, function(x) any(x == 3L))
ever_H <- apply(test_pop, 2, function(x) any(x == 4L))

#id agents with final state D
#death is terminal state, so last row of matrix ids dead agents
died <- test_pop[nrow(test_pop), ] == 6L

#calculate observed cumulative probabilities
#mean(ever_L) gives p(all agents who ever entered L), regardless
#of future progression to H, R, or D
observed_cumulative <- c(
  U = mean(ever_U),
  L = mean(ever_L),
  H = mean(ever_H),
  D = mean(died)
)

#combine expected and observed cumulative probabilities into summary table
cumulative_summary <- data.frame(
  outcome = names(expected_cumulative),
  expected_probability = as.numeric(expected_cumulative),
  observed_probability = as.numeric(observed_cumulative)
)

#calculate observed - expected probability
#difference should be small but do not need to be 0 (b/c stochasticiy)
cumulative_summary$difference <-
  cumulative_summary$observed_probability - cumulative_summary$expected_probability

#output table
print(cumulative_summary, row.names=FALSE)

#verify nested clinical progression pathways
#every agent who reaches L has to also reach U
#every agent who reaches H has to also reach L

stopifnot(
  all(!ever_L | ever_U),
  all(!ever_H | ever_L)
)

# =============================================================================
# POPULATION-LEVEL DIAGNOSTIC PLOTS
# =============================================================================

#######Clinical State Occupancy Over Time#######

#test_pop matrix
#rows = simulation hours (0-500)
#columns = individual agents (1-10,000)

#for each hour, count the number of agents occupying each clinical state
clinical_codes <- 1:6

state_counts <- t(
  apply(
    test_pop,
    1,
    function(x) tabulate(x, nbins = 6)
  )
)

#convert matrix to df
#rows = simulated hours (0-500)
#columns = number of agents in particular clinical state
state_counts <- as.data.frame(state_counts)
names(state_counts) <- state_name(clinical_codes)

#add simulation hour
state_counts$hour <- 0:(nrow(test_pop) -1L)

#check obs
head(state_counts)

#reshape wide --> long for plotting
#want: one row per hour*clinical state, number of agents in that state in a column
state_occupancy <- tidyr::pivot_longer(
  state_counts,
  cols = c(I, U, L, H, R, D),
  names_to = "clinical_state",
  values_to = "n"
)

#calculate P(agents in each clinical state)
#all 10,000 agents as denom
state_occupancy$proportion <-
  state_occupancy$n / ncol(test_pop)

#check
head(state_occupancy)

#make clinical_state into ordered factor for legend
state_occupancy$clinical_state <- factor(
  state_occupancy$clinical_state,
  levels = c("I", "U", "L", "H", "R", "D")
)

#plot proportion of agents in each clinical state over time
#each line is one clinical state
p_occupancy <- ggplot(
  state_occupancy,
  aes(x = hour, y = proportion, color = clinical_state)
) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Clinical State Occupancy Over Time",
    subtitle = "10,000 simulated infections",
    x = "Hours Since Initialization",
    y = "Proportion of Agents",
    color = "Clinical State"
  ) +
  theme_minimal()

p_occupancy

# =============================================================================
# FORMATTED CONDITIONAL TRANSITION PROBABILITY TABLE
# =============================================================================

#select columns
probability_table <- probability_summary[
  , c("transition",
      "expected_probability",
      "observed_probability")
]

#convert probabilities to percentages w/ 2 decimal places
probability_table$expected_probability <-
  sprintf("%.2f%%", probability_table$expected_probability * 100)

probability_table$observed_probability <-
  sprintf("%.2f%%", probability_table$observed_probability * 100)

#rename cols
names(probability_table) <- c(
  "Transition",
  "Expected",
  "Observed"
)

#view table
print(probability_table, row.names = FALSE)

#format
probability_gt <- probability_table %>%
  gt() %>%
  tab_header(
    title = "Conditional Clinical Transition Probabilities",
    subtitle = "10,000 simulated infections"
  ) %>%
  cols_align(
    align = "center",
    columns = c(Expected, Observed)
  ) %>%
  tab_source_note(
    source_note = "Placeholder transition probabilities; observed value from one stochastic simulation"
  )

probability_gt

gtsave(
  probability_gt,
  "conditional_transition_probabilities.png"
)

# =============================================================================
# FORMATTED CUMULATIVE SEVERITY TABLE
# =============================================================================

#select columns
cumulative_table <- cumulative_summary[
  , c("outcome",
      "expected_probability",
      "observed_probability")
]

#convert clinical state codes into descriptive labels
cumulative_table$outcome <- c(
  "Ever reached URTI",
  "Ever reached LRTI",
  "Ever reached SLRTI",
  "Death"
)

#format probabilities are percentages with 2 decimals
cumulative_table$expected_probability <-
  sprintf("%.2f%%", cumulative_table$expected_probability * 100)
cumulative_table$observed_probability <-
  sprintf("%.2f%%", cumulative_table$observed_probability * 100)

#rename cols
names(cumulative_table) <- c(
  "Cumulative Outcome",
  "Expected",
  "Observed"
)

#check
print(cumulative_table, row.names = FALSE)

#reformat
cumulative_gt <- cumulative_table %>%
  gt() %>%
  tab_header(
    title = "Cumulative Clinical Severity Outcomes",
    subtitle = "10,000 simulated infections"
  ) %>%
  cols_align(
    align = "center",
    columns = c(Expected, Observed)
  ) %>%
  cols_width(
    `Cumulative Outcome` ~ px(190),
    Expected ~ px(100),
    Observed ~ px(100)
  ) %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 18,
    heading.subtitle.font.size = 12,
    data_row.padding = px(10)
  ) %>%
  tab_source_note(
    source_note = paste(
      "Expected values derived from placeholder transition probabilities.",
      "Outcomes are not mutually exclusive."
    )
  )

cumulative_gt

gtsave(
  cumulative_gt,
  "cumulative_severity_table.png"
)
