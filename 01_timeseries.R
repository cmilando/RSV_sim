library(data.table)

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' get timeseries
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

pop_df <- readRDS("demo_pop.RDS")
head(pop_df)

## add asyptomatic
set.seed(1234)
pop_df$asymptomatic <- runif(nrow(pop_df)) < 0.1

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

Rcpp::sourceCpp("get_timeseries.cpp")
set.seed(123) # so certain random things always flip the same way

out <- get_timeseries(
  df_mat,
  ta_mat,
  hh_V     = 10,  # m^3
  school_V = 100, # m^3
  work_V   = 100, # m^3
  comm_V   = 100, # m^3
  n_days = as.integer(2),
  max_comm = as.integer(max(pop_df$community_id)),
  max_school =  as.integer(max(pop_df$school_id)),
  max_hh = as.integer(max(pop_df$household_id)),
  max_work = as.integer(max(pop_df$work_id))
)

conc = out$hh[1, ]
hhdt = data.table(conc, hour = 0:(length(conc) - 1))

library(ggplot2)
library(ggpubr)

ggplot(hhdt) + theme_classic2() +
  geom_line(aes(x = hour, y = conc), color = 'blue') +
  geom_vline(xintercept = c(7, 17, 23 + 8, 24 + 17),
             linetype = 'dashed', linewidth = 0.25) +
  ggtitle("household RSV concentration")


