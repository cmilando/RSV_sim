#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' SETUP -- re-running the pipeline using RCPP
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

library(data.table)
library(Rcpp)

# get files
sourceCpp("set_ids.cpp")

# some basic tests
intv = as.integer(1:10)
ransam_cpp(intv, 9)

# how large is the population
N <- 1e5

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' AGE DISTRIBUTION
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

library(data.table)

# Census Vintage 2024 national age-sex estimates
url <- paste0(
  "https://www2.census.gov/programs-surveys/popest/datasets/",
  "2020-2024/national/asrh/nc-est2024-agesex-res.csv"
)

dt <- fread(url)
table(dt$SEX)

# Total population (both sexes)
us_age <- dt[
  AGE < 999 & SEX == 0,
  .(age = AGE,
    population = POPESTIMATE2024)
]

table(us_age$age)
sum(us_age$population)

# Convert to proportions
us_age[, proportion := population / sum(population)]

head(us_age)

# get a representative sample
age_dist <- sample(us_age$age,
                   size = N,
                   prob = us_age$proportion,
                   replace = T)
hist(age_dist)


#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' CREATE DATA
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

# knowns
# adding 1 because these can never be 0
household_sizes <- rpois(N, 3) + 1  # comes from ACS
work_sizes      <- rpois(N, 20) + 1   # comes from BLS
school_sizes    <- rpois(N, 50) + 1  # comes from somewhere

# *******
# debug
# *******
# household_sizes <- rep(4, N )
# work_sizes <- rep(5, N )
# school_sizes <- rep(7, N )
# *********

# unknown
# this is just used to create the contact matrix
# we'll solve for this next
community_size_true <- rpois(N, 50)
stopifnot(all(community_size_true > 0))

# now create your population dataset
reset_pop_df <- function() {
  data.table(person_id = 1:N,
             age = round(age_dist),
             household_id = numeric(N),
             work_id = numeric(N),
             school_id = numeric(N),
             community_id = numeric(N))
}

pop_df <- reset_pop_df()
pop_df

# some basic tests
# fsubset_cpp(as.matrix(pop_df), 2, 0, 25, 3)

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' SET IDS
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

# Initialize
pop_df <- reset_pop_df()
names_vec <- names(pop_df)
pop_df
table(pop_df$age)

# test
# get_ids_cpp(
#   df = as.matrix(pop_df),
#   n = as.integer(5),
#   age_col = as.integer(2),
#   age_lower = 0,
#   age_upper = 25,
#   target_col = as.integer(3)
# )

# ***********************
# HOUSEHOLD
# household = 50% x (person < 25) and 50% x (person >= 25)
oo <- set_ids_cpp(
  df = as.matrix(pop_df),
  vec = as.integer(household_sizes),
  age_col = as.integer(2 - 1),         # do the C++ offset manually
  target_col = as.integer(3 - 1),      # do the C++ offset manually
  p1 = 0.5,
  p2 = 0.5,
  age0 = 0,
  age1 = 25,
  age2 = 125
)

pop_df <- as.data.table(oo)
pop_df

# ***********************
# WORK
# probably makes sense to have an upper age limit here
oo <- set_ids_single_cpp(
  df = as.matrix(pop_df),
  vec = as.integer(work_sizes),
  age_col = as.integer(2 - 1),
  target_col = as.integer(4 - 1),
  age0 = 20,
  age1 = 65
)

##
pop_df <- as.data.table(oo)
pop_df

# ***********************
# SCHOOL
oo <- set_ids_single_cpp(
   df = as.matrix(pop_df),
   vec = as.integer(school_sizes),
   age_col = as.integer(2 - 1),
   target_col = as.integer(5 - 1),
   age0 = 0,
   age1 = 20
)

##
pop_df <- as.data.table(oo)
head(pop_df)
table(pop_df$school_id, useNA = 'ifany')

# ***********************
# COMMUNITY
oo <- set_ids_single_cpp(
  df = as.matrix(pop_df),
  vec = as.integer(community_size_true),
  age_col = as.integer(2 - 1),
  target_col = as.integer(6 - 1),
  age0 = 0,
  age1 = 100
)

##
pop_df <- as.data.table(oo)
pop_df
table(pop_df$community_id)

# saveRDS(pop_df, "demo_pop.RDS")

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' get contact matrix
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

Rcpp::sourceCpp("contact_matrix.cpp")

true_contact_mat <- contact_matrix_cpp(
  age          = as.integer(pop_df$age),
  household_id = pop_df$household_id,
  max_hh       = as.integer(max(pop_df$household_id, na.rm = T)),
  work_id      = pop_df$work_id,
  max_work     = as.integer(max(pop_df$work_id, na.rm = T)),
  school_id    = pop_df$school_id,
  max_school   = as.integer(max(pop_df$school_id, na.rm = T)),
  community_id = pop_df$community_id,
  max_comm     = as.integer(max(pop_df$community_id, na.rm = T))
)

true_contact_mat <- as.data.table(true_contact_mat)
head(true_contact_mat, 10)
summary(true_contact_mat$ref_age)


library(ggplot2)

ggplot(true_contact_mat) +
  geom_tile(aes(x = ref_age, y = contact_age, fill = total)) +
  scale_fill_viridis_c()

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' Fit the contact matrix
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

N_pop <- length(age_dist)
N_pop

get_mat_by_lambda2 <- function(lambda_guess) {

  # ********************
  # lambda_guess = 10
  # ********************

  lambda_guess = round(lambda_guess)

  cat("> Lambda:", lambda_guess, "\n")

  comm_vect <-  rep(lambda_guess, N_pop / lambda_guess)

  if(N_pop %% lambda_guess > 0) {
    comm_vect = c(comm_vect, N_pop %% lambda_guess)
  }

  rand_rows <- sample(1:N_pop, N_pop)

  comm_id = 1
  rr_i  = 1
  mat_i = 1
  comm_id_mat <- matrix(nrow = N_pop, ncol = 1)

  for(i in 1:length(comm_vect)) {
    this_grp_size <- comm_vect[i]
    these_rows <- rand_rows[rr_i:(rr_i + this_grp_size - 1)]
    comm_id_mat[these_rows, mat_i] <- comm_id
    # update counters
    comm_id = comm_id + 1
    rr_i = rr_i + this_grp_size
  }

  # assumes you already have pop_df
  simple_pop_df <- pop_df
  simple_pop_df$community_id <- comm_id_mat

  # get the contact matrix
  test_contact_mat <- contact_matrix_cpp(
    age          = as.integer(simple_pop_df$age),
    household_id = simple_pop_df$household_id,
    max_hh       = as.integer(max(simple_pop_df$household_id, na.rm = T)),
    work_id      = simple_pop_df$work_id,
    max_work     = as.integer(max(simple_pop_df$work_id, na.rm = T)),
    school_id    = simple_pop_df$school_id,
    max_school   = as.integer(max(simple_pop_df$school_id, na.rm = T)),
    community_id = simple_pop_df$community_id,
    max_comm     = as.integer(max(simple_pop_df$community_id, na.rm = T))
  )

  test_contact_mat <- as.data.table(test_contact_mat)

  # METRICS
  MSD = mean((test_contact_mat$total - true_contact_mat$total)^2)

  xx <- 1 * MSD
  names(xx) = lambda_guess

  return(xx)

}

get_mat_by_lambda2(10)

#### ----------------------
### OPTIMZE
int_ternary_search <- function(f, low, high, max_iter = 5000, quiet = T) {

  iter <- 0
  prev_guesses <- c()

  while ((high - low) > 3 && iter < max_iter) {

    if(!quiet) cat("> Iter:", iter, "\n")
    if(!quiet) print(prev_guesses)

    m1 <- floor(low + (high - low) / 3)
    m2 <- floor(high - (high - low) / 3)

    if(m1 %notin% names(prev_guesses)) {
      if(!quiet) cat("    m1 new guess\n")
      f1 <- f(m1)
      prev_guesses[paste0(m1)] <- f1
    } else {
      if(!quiet) cat("    m1 use existing value\n")
      f1 <- prev_guesses[paste0(m1)]
    }

    if(m2 %notin% names(prev_guesses)) {
      if(!quiet) cat("    m2 new guess\n")
      f2 <- f(m2)
      prev_guesses[paste0(m2)] <- f2
    } else {
      if(!quiet) cat("    m2 use existing value\n")
      f2 <- prev_guesses[paste0(m2)]
    }

    if (f1 < f2) {
      high <- m2
    } else {
      low <- m1
    }

    iter <- iter + 1
  }

  # FINAL GUESSES
  if(!quiet) cat(">> FINALIZE\n")
  candidates <- low:high
  candidates[which.min(prev_guesses[paste0(candidates)])]
}

int_ternary_search(get_mat_by_lambda2, 2, 100)


### BRUTE FORCE
out_l <- lapply(c(2:100), get_mat_by_lambda2)

out_l

out_df_total <- do.call(c, out_l)
out_df_total_agg <- as.data.frame(out_df_total)
out_df_total_agg$lambda = as.integer(row.names(out_df_total_agg))
names(out_df_total_agg)[1] <- 'msd'

setDT(out_df_total_agg)
out_df_total_agg

ggplot(out_df_total_agg, aes(x = lambda,
                             y  = msd)) +
  geom_line() +
  geom_point() +
  ggpubr::theme_classic2()  +
  scale_y_log10()
