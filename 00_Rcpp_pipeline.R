#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' SETUP -- re-running the pipeline using RCPP
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

library(data.table)
library(Rcpp)

# get files
sourceCpp("fsubset.cpp")
sourceCpp("ransam.cpp")
sourceCpp("set_ids.cpp")

# some basic tests
intv = as.integer(1:10)
ransam_cpp(intv, 9)

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' CREATE DATA
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

N <- 1e1

# knowns
# adding 1 because these can never be 0
household_sizes <- rpois(N, 3) + 1  # comes from ACS
work_sizes      <- rpois(N, 20) + 1   # comes from BLS
school_sizes    <- rpois(N, 50) + 1  # comes from somewhere

household_sizes <- rep(4, N )
work_sizes <- rep(5, N )
school_sizes <- rep(5, N )

summary(household_sizes)

# unknown
# this is just used to create the contact matrix
# we'll solve for this next
community_size_true <- rpois(N, 50)
stopifnot(all(community_size_true > 0))

community_size_true <- rep(15, N )
community_size_true

# contact matrix
# ok so first you need an age distribution
# lets just assume its uniform
age_dist <- round(runif(N, min = 0, max = 100))
age_dist <- c(rep(15, N/2), rep(30, N/2))
hist(age_dist)
age_dist_df <- data.frame(table(age_dist))
names(age_dist_df)
saveRDS(age_dist, 'age_dist.RDS')

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
fsubset_cpp(as.matrix(pop_df), 2, 0, 25, 3)

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
get_ids_cpp(
  df = as.matrix(pop_df),
  n = as.integer(5),
  age_col = as.integer(2),
  age_lower = 0,
  age_upper = 25,
  target_col = as.integer(3)
)

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

