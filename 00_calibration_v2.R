#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' NOTES
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

# For each state, the way we will calibrate to the contact matrix is to have:
#
#   known:
#
#   poisson mu for househould size (basically, 1000 numbers since we know ACS)
# poisson mu for school size (again, known from ACS data)
# poisson mu for work size (and known from BLS etc data, 1000 numbers)
# time-activity of how people spend time between each place

# unknown:
#
#   poisson mu for community size

# target:
#
#   contact matrix

# need to figure out the correct units for p(exposure) but we have all the details we need

# the last component is using time-activity to merge them all
# to get one contact matrix

#
# We can do this in STAN pretty easily and just has to be done once.
#
# from this we'll know how large each community should be, and these will be randomly assigned once


#' ============================================================================

# Ways to make this faster
# * search among things that arent -1 so you pass in a smaller and smaller list
# * convert to a integer matrix
# * I suppose you could make this faster by randomly sampling and generating it
#   forwards rather than subsetting?



#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' CREATE DATA
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================
library(data.table)
set.seed(123)

system("rm *.o")
system("rm *.so")
system("R CMD SHLIB rsv.f90")

# load the library and create a tmp directory
# dyn.unload("rsv.so")
dyn.load("rsv.so")

# so N is the number of people represented
# this just has to be large enough
N <- 5e5

# knowns
# adding 1 because these can never be 0
household_sizes <- rpois(N, 3) + 1  # comes from ACS
work_sizes      <- rpois(N, 20) + 1   # comes from BLS
school_sizes    <- rpois(N, 50) + 1  # comes from somewhere

summary(household_sizes)

# unknown
# this is just used to create the contact matrix
# we'll solve for this next
community_size_true <- rpois(N, 50)

# contact matrix
# ok so first you need an age distribution
# lets just assume its uniform
age_dist <- runif(N, min = 0, max = 100)

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

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' SET IDS
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

# source("00_fcns.R")

pop_df <- reset_pop_df()
# og_pop_df <- pop_df
names_vec <- names(pop_df)
pop_df

# ***********************
# HOUSEHOLD
# household = 50% x (person < 25) and 50% x (person >= 25)
oo <- .Fortran("set_ids",
               df = as.matrix(pop_df),
               nrows = as.integer(nrow(pop_df)),
               ncols = as.integer(ncol(pop_df)),
               age_col = as.integer(2),
               vec = as.integer(household_sizes),
               zero_col = as.integer(3),
               p1 = 0.5,
               p2 = 0.5,
               age0 = 0,
               age1 = 25,
               age2 = 125)

##
pop_df <- as.data.table(oo$df)
head(pop_df)
subset( pop_df, age < 25 & household_id < 0)

##
rr <- which(pop_df[, 3] < 0)
pop_df[rr, 3] <- NA
subset( pop_df, age < 25 & is.na(household_id))

# so the reason there are so many NAs is that there aren't any
# people young enough to make any more houses
# hm this doesn't seem to be true
subset(pop_df, age > 25 & is.na(household_id))

# subset
pop_df <- subset(pop_df, !is.na(household_id))
pop_df


# check the size distribution
x1 <- pop_df[, .N, by = household_id]
x1 <- table(x1$N)

x0 <- table(household_sizes)

source("00_fcns.R")
plot_dists(x0, x1)


# ***********************
# SCHOOL
# >> needs to be a single
# school = 80% (person < 20) and 20% (person > = 20)
# DOESN'T SEEM TO BE WORKING YET .... THERE SHOULD BE LIKE 10 schools
# pop_df <- set_ids(local_pop_df = pop_df,
#                   vec = school_sizes,
#                   column = "school_id",
#                   p1 = 0.8, p2 = 0.2,
#                   age0 = 0, age1 = 20, age2 = 125)

oo <- .Fortran("set_ids_single",
               df = as.matrix(pop_df),
               nrows = as.integer(nrow(pop_df)),
               ncols = as.integer(ncol(pop_df)),
               age_col = as.integer(2),
               vec = as.integer(school_sizes),
               zero_col = as.integer(5),
               age0 = 0,
               age1 = 20)

##
pop_df <- as.data.table(oo$df)
head(pop_df)

# check the size distribution
x1 <- pop_df[pop_df$age < 20, .N, by = school_id]
x1 <- table(x1$N)
x1

x0 <- table(school_sizes)

plot_dists(x0, x1)

# ***********************
# WORK
# work = 100% (person > 20)
# tmp reset NA to -999
oo <- .Fortran("set_ids_single",
               df = as.matrix(pop_df),
               nrows = as.integer(nrow(pop_df)),
               ncols = as.integer(ncol(pop_df)),
               age_col = as.integer(2),
               vec = as.integer(work_sizes),
               zero_col = as.integer(4),
               age0 = 20,
               age1 = 125)

##
pop_df <- as.data.table(oo$df)
head(pop_df)

# check the size distribution
x1 <- pop_df[pop_df$age >= 20, .N, by = work_id]
x1 <- table(x1$N)

x0 <- table(work_sizes)

plot_dists(x0, x1)


# ***********************
# COMMUNITY
# community = all people
df_backup <- pop_df

oo <- .Fortran("set_ids_single",
               df = as.matrix(pop_df),
               nrows = as.integer(nrow(pop_df)),
               ncols = as.integer(ncol(pop_df)),
               age_col = as.integer(2),
               vec = as.integer(community_size_true),
               zero_col = as.integer(6),
               age0 = 0,
               age1 = 125)

pop_df <- as.data.table(oo$df)
head(pop_df)

summary(pop_df$community_id)

# ***********************
# set NAs
for(j in 3:6) {
  rr <- which(pop_df[, ..j] < 0)
  if(length(rr) > 0) pop_df[rr, j] <- NA
}
head(pop_df)

# ***********************
plot_dist <- function(str_id, vec) {
  x1 <- pop_df[!is.na(get(str_id)), .N, by = str_id]
  x1 <- table(x1$N)
  x0 <- table(vec)
  plot_dists(x0, x1)
}
plot_dist("community_id", community_size_true)
plot_dist("work_id", work_sizes)
plot_dist("household_id", household_sizes)
plot_dist("school_id", school_sizes)

saveRDS(pop_df, "demo_pop.RDS")
