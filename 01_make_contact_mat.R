
library(data.table)

pop_df <- readRDS("demo_pop.RDS")

head(pop_df)
dim(pop_df)

# first do some cleaning that there should not be more than 2 missings in any row
n_miss <- apply(pop_df, 1, \(x) length(which(is.na(x))))
rr <- which(n_miss > 1)
rr
pop_df <- pop_df[n_miss == 1, ]

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

GET_COMMUNITY <- T

oo <- vector("list", nrow(pop_df))

get_oo <- function(i) {

  fxgrid <- tryCatch({

  person_i <- pop_df[i, ]
  person_i

  hh_id <-  person_i$household_id
  w_id <-  person_i$work_id
  s_id <-  person_i$school_id
  c_id <-  person_i$community_id
  if(!GET_COMMUNITY) c_id <- NA

  household_members <- work_members <- school_members <- community_members <- NA

  # removed self so no double counting

  # HOUSEHOLD
  household_members <- subset(pop_df, household_id == hh_id &
                                person_id != person_i$person_id)
  if(nrow(household_members) == 0) hh_id <- NA

  # WORK
  if(!is.na(w_id)) {
    work_members <- subset(pop_df, work_id == w_id &
                             person_id != person_i$person_id)
    if(nrow(work_members) == 0) w_id <- NA
  }

  # SCHOOL
  if(!is.na(s_id)) {
    school_members <- subset(pop_df, school_id == s_id &
                               person_id != person_i$person_id)
    if(nrow(school_members) == 0) s_id <- NA
  }

  # COMMUNITY
  if(!is.na(c_id)) {
    community_members <- subset(pop_df, community_id == c_id &
                                  person_id != person_i$person_id)
    if(nrow(community_members) == 0) c_id <- NA
  }


  # just get number of people interacted with
  # you could include time-activity if you wanted to
  # but since each is like 8 hours you are fine

  x1 <- x2 <- x3 <- x4 <- NA
  if(!is.na(hh_id)) x1 <- table(household_members$age)
  if(!is.na(c_id))  x2 <- table(community_members$age)
  if(!is.na(s_id))  x3 <- table(school_members$age)
  if(!is.na(w_id))  x4 <- table(work_members$age)

  make_df <- function(xx) {
    y <- as.data.table(xx)
    names(y) = c('age', deparse(substitute(xx)))
    y$age <- as.numeric(y$age)
    y
  }

  # this should have the same upper limit as your other script
  xgrid <- as.data.table(tidyr::expand_grid(age = as.numeric(0:100)))

  if(!is.na(hh_id)) x1 <- make_df(x1)
  if(!is.na(c_id))  x2 <- make_df(x2)
  if(!is.na(s_id))  x3 <- make_df(x3)
  if(!is.na(w_id))  x4 <- make_df(x4)

  if(!is.na(hh_id)) xgrid <- x1[xgrid, on = 'age']
  if(!is.na(c_id))  xgrid <- x2[xgrid, on = 'age']
  if(!is.na(s_id))  xgrid <- x3[xgrid, on = 'age']
  if(!is.na(w_id))  xgrid <- x4[xgrid, on = 'age']

  for(j in 2:ncol(xgrid)) {
    rr <- which(is.na(xgrid[, ..j]))
    if(length(rr) > 0) xgrid[rr, j] <- 0
  }

  xgrid

  # collapses across all environments
  xgrid$pt_sum <- apply(xgrid[,2:ncol(xgrid)], 1, sum)

  xgrid$ref_age <- person_i$age
  xgrid$ref_id <- person_i$person_id

  xgrid$contact_age <- xgrid$age

  xgrid <- xgrid[, .(ref_id, ref_age, contact_age, pt_sum)]

  # subset to just non-empty rows
  # xgrid <- subset(xgrid, pt_sum > 0)

  xgrid

  }, error = function(msg){
    stop(paste0("ERROR AT i = ", i))
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

# takes a few minutes but not terrible3
x_l <- my_fcn(1:10)

# #333
x_l <- my_fcn(1:nrow(pop_df))

# save
saveRDS(x_l, paste0("x_l_", GET_COMMUNITY, ".RDS"))

