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
source("00_fcns.R")
set.seed(123)

system("rm *.o")
system("rm *.so")
system("R CMD SHLIB rsv.f90")

# load the library and create a tmp directory
# dyn.unload("rsv.so")
dyn.load("rsv.so")

# so N is the number of people represented
# this just has to be large enough

# the smallest number you can start with seems to be
# 300
N <- 1000

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

# right so make everyone else either part of a 2 or a 1
# subset( pop_df, age > 25 & household_id < 0)

# wait is this a faster way to do this
# get everyone who is left
# randomize the order
# then just go down the line assigning groups
# its just one randomize then a list
# that has to be way faster
rr <- which(pop_df$household_id < 0)
rr
if(length(rr) > 0) {
  length(rr) %% 3 # ok so this means it ends with a 2
  n_grps <- c(rep(c(2, 1), floor(length(rr) / 3)), length(rr) %% 3)
  stopifnot(sum(n_grps) == length(rr))

  # now randomize
  rand_rows <- sample(rr, length(rr))

  # and go down the line
  hh_id <- max(pop_df$household_id) + 1
  rr_i = 1
  for(i in 1:length(n_grps)) {
    this_grp_size <- n_grps[i]
    these_rows <- rand_rows[rr_i:(rr_i + this_grp_size - 1)]
    pop_df$household_id[these_rows] <- hh_id
    # update counters
    hh_id = hh_id + 1
    rr_i = rr_i + this_grp_size
  }
  pop_df
}

# and its probably even faster to do in the big group too
# just split them into two lists and go down each group
# you know it might still be faster this way
# but i think doing both seems fair

# check the size distribution
# x1 <- pop_df[, .N, by = household_id]
# x1 <- table(x1$N)
#
# x0 <- table(household_sizes)
#
# plot_dists(x0, x1, 'household')


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
table(pop_df$school_id)

#
# # set <0 to NA
# rr <- which(pop_df$school_id < 0 & pop_df$age < 20)
# length(rr)
#
# # check the size distribution
# x1 <- pop_df[pop_df$age <= 20, .N, by = school_id]
# x1 <- table(x1$N)
# x1å
#
# x0 <- table(school_sizes)
#
# plot_dists(x0, x1, 'school')

# pop_df$school_id[1:5] = 1
# pop_df$school_id[6:10] = 2

# ***********************
# WORK
# work = 100% (person > 20)
# tmp reset NA to -999
# >> Set the upper bound to be
oo <- .Fortran("set_ids_single",
               df = as.matrix(pop_df),
               nrows = as.integer(nrow(pop_df)),
               ncols = as.integer(ncol(pop_df)),
               age_col = as.integer(2),
               vec = as.integer(work_sizes),
               zero_col = as.integer(4),
               age0 = 20,
               age1 = 100)

##
pop_df <- as.data.table(oo$df)
head(pop_df)
table(pop_df$work_id)

#
# # check the size distribution
# x1 <- pop_df[pop_df$age >= 20, .N, by = work_id]
# x1 <- table(x1$N)
#
# x0 <- table(work_sizes)
#
# plot_dists(x0, x1, 'work')


# pop_df$work_id[11:13] = 1
# pop_df$work_id[14:17] = 2
# pop_df$work_id[18:20] = 3

# ***********************
# Get rid of anyone that doesn't have school or work?
# what about people > 65?

# ***********************
# COMMUNITY
# community = all people
oo <- .Fortran("set_ids_single",
               df = as.matrix(pop_df),
               nrows = as.integer(nrow(pop_df)),
               ncols = as.integer(ncol(pop_df)),
               age_col = as.integer(2),
               vec = as.integer(community_size_true),
               zero_col = as.integer(6),
               age0 = 0,
               age1 = 100)

pop_df <- as.data.table(oo$df)
head(pop_df)
#
# # add the extra community
rr <- which(pop_df$community_id < 0)
rr
if(length(rr) > 0) {
  max_cid <- max(pop_df$community_id, na.rm = T)
  pop_df$community_id[rr] <- max_cid + 1
  head(pop_df)
}
#
# # subset
# # everyone needs a community
# summary(pop_df$community_id)

# rr <- sample(1:20, 10)
#
# pop_df$community_id[1:20 %in% rr] = 1
# pop_df$community_id[!(1:20 %in% rr)] = 2

# ***********************
# set NAs
# for(j in 3:6) {
#   rr <- which(pop_df[, ..j] < 0)
#   if(length(rr) > 0) pop_df[rr, j] <- NA
# }
# head(pop_df)

# ***********************
# plot_dist <- function(str_id, vec) {
#   x1 <- pop_df[!is.na(get(str_id)), .N, by = str_id]
#   x1 <- table(x1$N)
#   x0 <- table(vec)
#   plot_dists(x0, x1, str_id)
# }
# p1 <- plot_dist("household_id", household_sizes)
# p2 <- plot_dist("school_id", school_sizes)
# p3 <- plot_dist("work_id", work_sizes)
# p4 <- plot_dist("community_id", community_size_true)

# library(patchwork)
# p1 + p2 + p3 + p4 + plot_layout(guides = 'collect', axes = 'collect')

rr <- which(pop_df$work_id < 0)
pop_df$work_id[rr] <- NA

rr <- which(pop_df$school_id < 0)
pop_df$school_id[rr] <- NA

saveRDS(pop_df, "demo_pop.RDS")
