#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' SETUP -- re-running the pipeline using RCPP
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

library(data.table)
library(Rcpp)

# get files
sourceCpp("set_ids.cpp")

ransam_cpp(1:10, 3)

# how large is the population
N <- 1e5

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' AGE DISTRIBUTION
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

# Census Vintage 2024 national age-sex estimates
# url <- paste0(
#   "https://www2.census.gov/programs-surveys/popest/datasets/",
#   "2020-2024/national/asrh/nc-est2024-agesex-res.csv"
# )

# dt <- fread(url)

dt <- data.table::fread(file = "nc-est2024-agesex-res.csv")
table(dt$SEX)

# Total population (both sexes)
us_age <- dt[
  AGE < 999 & SEX == 0,
  .(age = AGE,
    population = POPESTIMATE2024)
]

# seems like this is just 0-100 ages
table(us_age$age)
sum(us_age$population)

# Convert to proportions
us_age[, proportion := population / sum(population)]

head(us_age)
sum(us_age$population) / 1e6

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
set.seed(1234)
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
community_size_true <- rpois(N, 50) + 1
stopifnot(all(community_size_true > 0))

# now create your population dataset
# updating the person ID to be 0 to N
reset_pop_df <- function() {
  data.table(person_id = (0:(N-1)),
             age = round(age_dist),
             household_id = numeric(N) - 1,
             work_id = numeric(N) - 1,
             school_id = numeric(N) - 1,
             community_id = numeric(N) - 1)
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
  age2 = 100
)

pop_df <- as.data.table(oo)
pop_df
min(pop_df$household_id)
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

saveRDS(pop_df, "demo_pop.RDS")

