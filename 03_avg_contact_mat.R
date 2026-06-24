#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================
library(data.table)
library(ggplot2)

# AGE DIST
age_dist <- readRDS("age_dist.RDS")
age_dist_df <- data.frame(table(age_dist))
setDT(age_dist_df)
age_dist_df$age_dist <- as.numeric(as.character(age_dist_df$age_dist))
age_dist_df$Freq <- as.numeric(age_dist_df$Freq)
setnames(age_dist_df, "age_dist", "contact_age")
age_dist_df

get_contact_plot <- function(GET_COMMUNITY) {

  # ******
  # GET_COMMUNITY = T
  # *****

  x_l <- readRDS(paste0("x_l_", GET_COMMUNITY, ".RDS"))

  x_df <- do.call(rbind, x_l)

  head(x_df)
  dim(x_df)

  # merge
  x_df <- age_dist_df[
    x_df, on  = 'contact_age'
  ]

  dim(x_df)
  head(x_df)

  sub_x_df <- subset(x_df, !is.na(Freq))

  head(sub_x_df)

  # where do you actually have data
  write.table(sub_x_df,
              paste0("x_df_", GET_COMMUNITY, ".csv"),
              quote = F, row.names = F, sep = "|")

  #
  group_cols = c(
    'ref_age', 'contact_age', 'Freq'
  )


  # how many people you are interacting with
  # what if you interact with the same person in multiple environments?

  # so at the person level is c(1,0)
  # and then you average up

  # volume version vs a probability version

  # (1) network analysis approach
  # (2) age-stratified approach without networks

  # x_df_agg <- x_df[, .(
  #   sum_pt_sum = sum(pt_sum)), by = group_cols
  # ]

  x_df_agg <- subset(x_df, !is.na(Freq))[, .(
    mean_pt_sum = sum(pt_sum)), by = group_cols
  ]

  subset(x_df_agg, mean_pt_sum > 0)

  # reset 0 to NA
  # rr <- which(x_df_agg$mean_pt_sum == 0)
  # x_df_agg$mean_pt_sum[rr] <- NA

  return(x_df_agg)

}

total_contact <- get_contact_plot(GET_COMMUNITY = T)

true_contact_mat <- subset(total_contact, mean_pt_sum > 0)
true_contact_mat

ggplot(total_contact) +
  geom_tile(aes(x = contact_age, y = ref_age, fill = mean_pt_sum),
            color = 'white', linewidth = 0.05) +
  scale_fill_viridis_c() + ggtitle("Full Contact Matrix")

total_contact_no_comm <- get_contact_plot(GET_COMMUNITY = F)
subset(total_contact_no_comm, mean_pt_sum > 0)

ggplot(total_contact_no_comm) +
  geom_tile(aes(x = contact_age, y = ref_age, fill = mean_pt_sum),
            color = 'white', linewidth = 0.005) +
  scale_fill_viridis_c() + ggtitle("Contact Matrix - No Community Contact")

# who acquires infection from whom
# you have interacted with this person
# not necessarily scaled by volume
# when you later on have a transmission probability
# and then you add the volume post-hoc, there is greater volume
# based on what network each person is in

# # lets dig into this a little more
# # what does it mean to have mean_pt_sum of 8
# rr <- which.max(total_contact$mean_pt_sum)
#
# data.frame(total_contact[rr, ])
# # ok so 15 year olds
# # have a mean contact with 10 year olds of 8
#
# subset(pop_df, age == 3)
#
# subset(pop_df, school_id == 1 & age == 15)
# subset(pop_df, school_id == 1 & age == 10)
# # ok but thats 4 each, not 8 each
#
# subset(pop_df, age == 10)

# what is the mean difference between the two
# setnames(total_contact_no_comm, "mean_pt_sum", "nc_mean_pt_sum")
#
# comb_df <- total_contact_no_comm[
#   total_contact, on = c('ref_age', 'contact_age', 'Freq')
# ]
#
# dim(comb_df)
# head(comb_df)
#
# mean(comb_df$mean_pt_sum - comb_df$nc_mean_pt_sum)
# at n = 300 this is 0.4
# it should be higher right? or maybe not?
# maybe you need to simplify the age distribution even more

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

# since community is everyone can we just take a random population sample
# then you don't have to do the individual accounting

# basically you could get the average contact matrix without community size
# and then you could get the final contact matrix from the literature presumably

# (1) assume community size lambda
# (2) then sample 100 draws of that size from the age distribution
# (3) then create a contact matrix addition
# (4) then add it to the x without community size distribution
# (5) then compare it to the target and adjust lambda

# well actually, you might not need STAN right, because
# its just one parameters

# why don't you start with NLOPTR and then you can do others later

# you know lambda_true is 50

# this isn't exactly correct because

#' ============================================================================
# library(data.table)
# library(ggplot2)
# library(future)
# library(future.apply)
# library(progressr)
#
# # setup for parallel processing
# plan(multisession, workers = 4)
# nbrOfWorkers()
# set.seed(1)
# handlers(global = TRUE)
# handlers("progress")
#
# age_dist <- readRDS("age_dist.RDS")
# N_pop <- length(age_dist)
# age_dist
#
# get_mat_by_lambda <- function(lambda_guess) {
#
#   # ********************
#   # lambda_guess = 10
#   # ********************
#
#   comm_vect <-  rep(lambda_guess, N_pop / lambda_guess)
#
#   if(N_pop %% lambda_guess > 0) {
#     comm_vect = c(comm_vect, N_pop %% lambda_guess)
#   }
#
#   comm_vect
#   rand_rows <- sample(1:N_pop, N_pop)
#   rand_rows
#
#   comm_id <-  1
#   rr_i = 1
#   comm_id_mat <- matrix(nrow = N_pop, ncol = 1)
#   mat_i = 1
#
#   for(i in 1:length(comm_vect)) {
#     this_grp_size <- comm_vect[i]
#     these_rows <- rand_rows[rr_i:(rr_i + this_grp_size - 1)]
#     comm_id_mat[these_rows, mat_i] <- comm_id
#     # update counters
#     comm_id = comm_id + 1
#     rr_i = rr_i + this_grp_size
#   }
#
#   comm_id_mat
#
#   age_dist
#
#   simple_pop_df <- data.table(
#     person_id = 1:N_pop,
#     age = age_dist,
#     community_id = comm_id_mat[, 1]
#   )
#
#   get_simple_oo <- function(i) {
#
#     fxgrid <- tryCatch({
#
#     # FOR EACH I
#     person_i <- simple_pop_df[i, ]
#
#     c_id <-  person_i$community_id
#
#     community_members <- subset(simple_pop_df, community_id == c_id &
#                                   person_id != person_i$person_id)
#
#     x2 <- table(community_members$age)
#
#     make_df <- function(xx) {
#       if(length(xx) > 1) {
#         y <- as.data.table(xx)
#         names(y) = c('age', deparse(substitute(xx)))
#         y$age <- as.numeric(y$age)
#         return(y)
#       } else {
#         y <- data.table(age = as.numeric(0:100), v = 0)
#         y$v <- as.numeric(y$v)
#         names(y) = c('age', deparse(substitute(xx)))
#         return(y)
#       }
#     }
#
#     xgrid <- make_df(x2)
#
#     # collapses across all environments
#     setnames(xgrid, 'x2', 'comm_sum')
#
#     xgrid$ref_age <- person_i$age
#     xgrid$ref_id  <- person_i$person_id
#
#     setnames(xgrid, 'age', 'contact_age')
#     xgrid
#
#     # commenting this out so can trace the sum
#     # xgrid <- xgrid[, .(ref_id, ref_age, contact_age, pt_sum)]
#
#
#     # subset to just non-empty rows
#     # xgrid <- subset(xgrid, pt_sum > 0)
#
#     xgrid
#
#     }, error = function(msg){
#       stop(paste0("ERROR AT i = ", i))
#     })
#
#     return(fxgrid)
#
#   }
#
#   my_fcn <- function(p_all) {
#     p <- progressor(along = p_all)
#     xx <- lapply(p_all, function(x) {
#       p(sprintf("x=%s", x))
#       get_simple_oo(x)
#     })
#     return(xx)
#   }
#
#   # takes a few minutes but not terrible3
#   x_l <- my_fcn(1:10)
#   x_l
#
#   # #333
#   x_l <- my_fcn(1:N_pop)
#   x_df <- do.call(rbind, x_l)
#
#   group_cols = c(
#     'ref_age', 'contact_age'
#   )
#
#   x_df_agg <- x_df[, .(
#     mean_comm_sum = sum(comm_sum)), by = group_cols
#   ]
#
#   out_df <- subset(x_df_agg, mean_comm_sum > 0)
#   out_df$lambda <- lambda_guess
#   out_df
#
#   out_df <- true_contact_mat[
#     out_df, on = c('ref_age', 'contact_age')
#   ]
#
#   out_df
#
#   return(out_df)
#
# }
#
# out_l <- lapply(c(2:20), get_mat_by_lambda)
# out_df_total <- do.call(rbind, out_l)
#
# library(tidyverse)
# out_df_total_agg <- out_df_total %>%
#   mutate(diff = mean_pt_sum - mean_comm_sum) %>%
#   group_by(lambda) %>%
#   summarize(
#     .groups = 'keep',
#     abs_mean_diff = abs(mean(diff)),
#     sd_diff = sd(diff)
#   )
#
# out_df_total_agg
#
# ggplot(out_df_total_agg, aes(x = factor(lambda),
#                              y  = abs_mean_diff,
#                              group = 1)) +
#   geom_line() + geom_point()


## ============================================================================
## ////////////////////////////////////////////////////////////////////////////
## ============================================================================

## hmm. the above doesn't work, so try reformatting to the true pop_df?
library(data.table)
library(ggplot2)
# library(future)
# library(future.apply)
library(progressr)
#
# # setup for parallel processing
# plan(multisession, workers = 4)
# nbrOfWorkers()
# set.seed(1)
# handlers(global = TRUE)
# handlers("progress")

age_dist <- readRDS("age_dist.RDS")
N_pop <- length(age_dist)
age_dist

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

  comm_vect
  rand_rows <- sample(1:N_pop, N_pop)
  rand_rows

  comm_id <-  1
  rr_i = 1
  comm_id_mat <- matrix(nrow = N_pop, ncol = 1)
  mat_i = 1

  for(i in 1:length(comm_vect)) {
    this_grp_size <- comm_vect[i]
    these_rows <- rand_rows[rr_i:(rr_i + this_grp_size - 1)]
    comm_id_mat[these_rows, mat_i] <- comm_id
    # update counters
    comm_id = comm_id + 1
    rr_i = rr_i + this_grp_size
  }

  table(comm_id_mat)

  table(age_dist)

  simple_pop_df <- readRDS("demo_pop.RDS")

  simple_pop_df$community_id <- comm_id_mat

  get_oo <- function(i) {

    fxgrid <- tryCatch({

    #i = 1

    person_i <- simple_pop_df[i, ]
    person_i

    hh_id <-  person_i$household_id
    w_id <-  person_i$work_id
    s_id <-  person_i$school_id
    c_id <-  person_i$community_id

    household_members <- work_members <- school_members <- community_members <- NA

    # removed self so no double counting

    # HOUSEHOLD
    household_members <- subset(simple_pop_df, household_id == hh_id &
                                  person_id != person_i$person_id)
    if(nrow(household_members) == 0) hh_id <- NA

    # WORK
    if(!is.na(w_id)) {
      work_members <- subset(simple_pop_df, work_id == w_id &
                               person_id != person_i$person_id)
      if(nrow(work_members) == 0) w_id <- NA
    }

    # SCHOOL
    if(!is.na(s_id)) {
      school_members <- subset(simple_pop_df, school_id == s_id &
                                 person_id != person_i$person_id)
      if(nrow(school_members) == 0) s_id <- NA
    }

    # COMMUNITY
    if(!is.na(c_id)) {
      community_members <- subset(simple_pop_df, community_id == c_id &
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
      if(length(xx) > 1) {
        y <- as.data.table(xx)
        names(y) = c('age', deparse(substitute(xx)))
        y$age <- as.numeric(y$age)
        return(y)
      } else {
        y <- data.table(age = as.numeric(0:100), v = 0)
        y$v <- as.numeric(y$v)
        names(y) = c('age', deparse(substitute(xx)))
        return(y)
      }
    }

    # this should have the same upper limit as your other script
    xgrid <- data.table(age = as.numeric(0:100))

    x1 <- make_df(x1)
    x2 <- make_df(x2)
    x3 <- make_df(x3)
    x4 <- make_df(x4)

    xgrid <- x1[xgrid, on = 'age']
    xgrid <- x2[xgrid, on = 'age']
    xgrid <- x3[xgrid, on = 'age']
    xgrid <- x4[xgrid, on = 'age']

    for(j in 2:ncol(xgrid)) {
      rr <- which(is.na(xgrid[, ..j]))
      if(length(rr) > 0) xgrid[rr, j] <- 0
    }

    xgrid

    # collapses across all environments
    xgrid$pt_sum <- apply(xgrid[,2:ncol(xgrid)], 1, sum)

    xgrid$ref_age <- person_i$age
    xgrid$ref_id  <- person_i$person_id

    setnames(xgrid, 'age', 'contact_age')
    setnames(xgrid, 'x1', 'household')
    setnames(xgrid, 'x2', 'community')
    setnames(xgrid, 'x3', 'school')
    setnames(xgrid, 'x4', 'work')

    subset(xgrid, pt_sum > 0)

    xgrid


    }, error = function(msg){
      stop(paste0("ERROR AT i = ", i, msg))
    })

    return(fxgrid)

  }

  my_fcn <- function(p_all) {
    # p <- progressor(along = p_all)
    xx <- lapply(p_all, function(x) {
      # p(sprintf("x=%s", x))
      get_oo(x)
    })
    return(xx)
  }

  # takes a few minutes but not terrible3
  x_l <- my_fcn(1:nrow(simple_pop_df))

  x_df <- do.call(rbind, x_l)
  head(x_df)

  group_cols = c(
    'ref_age', 'contact_age'
  )

  x_df_agg <- x_df[, .(
    test_mean_pt_sum = sum(pt_sum)), by = group_cols
  ]

  out_df <- subset(x_df_agg, test_mean_pt_sum > 0)
  out_df$lambda <- lambda_guess
  out_df


  out_df <- true_contact_mat[
    out_df, on = c('ref_age', 'contact_age')
  ]

  out_df

  # now get the value
  out_df_total_agg <- out_df[
    , .(msd = mean((mean_pt_sum - test_mean_pt_sum)^2),
        sd = sd((mean_pt_sum - test_mean_pt_sum)^2)),
    by = lambda
  ]

  xx <- 1 * out_df_total_agg$msd
  names(xx) = lambda_guess

  return(xx)

}

#### ----------------------
### OPTIMZE
int_ternary_search <- function(f, low, high, max_iter = 50, quiet = T) {

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

int_ternary_search(get_mat_by_lambda2, 2, 20)


### BRUTE FORCE
out_l <- lapply(c(2:20), get_mat_by_lambda2)

out_l

out_df_total <- do.call(c, out_l)
out_df_total_agg <- as.data.frame(out_df_total)
out_df_total_agg$lambda = as.integer(row.names(out_df_total_agg))
names(out_df_total_agg)[1] <- 'msd'

setDT(out_df_total_agg)
out_df_total_agg

ggplot(out_df_total_agg, aes(x = factor(lambda),
                             y  = msd,
                             group = 1)) +
  geom_line() +
  geom_point() +
  ggpubr::theme_classic2()  +
  scale_y_log10()
