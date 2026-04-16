
library(data.table)

pop_df <- readRDS("demo_pop.RDS")

head(pop_df)
dim(pop_df)


# ok so now you need to
# write some STAN code
# but you need a contact matrix right?

# so now you need to introduce time-activity so you can calculate
# a contact matrix

# maybe its just the number of people that each person comes into contact
# or the person-time
# with in each age group in each place
# so we have age x age in each location

# could do this in parallel

i = 1

oo <- vector("list", nrow(pop_df))

get_oo <- function(i) {

  fxgrid <- tryCatch({

  person_i <- pop_df[i, ]

  hh_id <-  person_i$household_id
  w_id <-  person_i$work_id
  s_id <-  person_i$school_id
  c_id <-  person_i$community_id

  household_members <- work_members <- school_members <- community_members <- NA
  household_members <- subset(pop_df, household_id == hh_id)
  if(!is.na(w_id)) work_members <- subset(pop_df, work_id == w_id)
  if(!is.na(s_id)) school_members <- subset(pop_df, school_id == s_id)
  community_members <- subset(pop_df, community_id == c_id)

  # time-activity
  x1 <- x2 <- x3 <- x4 <- NA
  x1 <- table(household_members$age)               * 8 #hours
  x2 <- table(community_members$age)               * 8 #hours
  if(!is.na(s_id)) x3 <- table(school_members$age) * 8 #hours
  if(!is.na(w_id)) x4 <- table(work_members$age)   * 8 #hours

  make_df <- function(xx) {
    y <- as.data.table(xx)
    names(y) = c('age', deparse(substitute(xx)))
    y$age <- as.numeric(y$age)
    y
  }

  xgrid <- as.data.table(tidyr::expand_grid(age = as.numeric(0:125)))

  x1 <- make_df(x1)
  x2 <- make_df(x2)
  if(!is.na(s_id)) x3 <- make_df(x3)
  if(!is.na(w_id)) x4 <- make_df(x4)

  xgrid <- x1[xgrid, on = 'age']
  xgrid <- x2[xgrid, on = 'age']
  if(!is.na(s_id)) xgrid <- x3[xgrid, on = 'age']
  if(!is.na(w_id)) xgrid <- x4[xgrid, on = 'age']

  for(j in 2:ncol(xgrid)) {
    rr <- which(is.na(xgrid[, ..j]))
    if(length(rr) > 0) xgrid[rr, j] <- 0
  }

  xgrid$pt_sum <- apply(xgrid[,2:ncol(xgrid)], 1, sum)

  xgrid$ref_age <- person_i$age

  xgrid <- xgrid[, .(age, ref_age, pt_sum)]

  xgrid
  }, error = function(msg){
    stop(paste0("ERROR AT", i))
  })
  return(fxgrid)

}

library(future)
library(future.apply)
library(progressr)

# setup for parallel processing
plan(multisession)
set.seed(1)
handlers(global = TRUE)
handlers("progress")

my_fcn <- function(p_all) {
  p <- progressor(along = p_all)
  xx <- future_lapply(p_all, function(x) {
    p(sprintf("x=%s", x))
    get_oo(x)
  })
  return(xx)
}

# takes a few minutes but not terrible
x_l <- my_fcn(1:10)
x_l <- my_fcn(1:nrow(pop_df))
x_l[[1]]

