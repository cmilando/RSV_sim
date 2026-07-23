library(data.table)

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' get timeseries
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

Rcpp::sourceCpp("get_timeseries.cpp")

pop_df <- readRDS("demo_pop.RDS")
head(pop_df)

time_activity = data.table(
  hour = 0:23,
  household = rep(0, 24),
  work_school = rep(0, 24),
  community = rep(0, 24))

df_mat <- as.matrix(pop_df)
mode(df_mat) <- 'integer'
df_mat

ta_mat <- as.matrix(time_activity)
mode(ta_mat) <- 'integer'
ta_mat

out <- get_timeseries(
  df_mat,
  ta_mat,
  hh_V     = 100,
  school_V = 100,
  work_V   = 100,
  comm_V   = 100,
  n_days = as.integer(1),
  max_comm = as.integer(max(pop_df$community_id)),
  max_school =  as.integer(max(pop_df$school_id)),
  max_hh = as.integer(max(pop_df$household_id)),
  max_work = as.integer(max(pop_df$work_id))
)

dim(out$hh)

head(out$hh)
