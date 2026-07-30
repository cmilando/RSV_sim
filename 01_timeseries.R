library(data.table)
library(jsonlite)

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' get timeseries
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

pop_df <- readRDS("demo_pop.RDS")
head(pop_df)
dim(pop_df)

# set up some things to track
subset(pop_df, household_id == 0)
person_IDs = subset(pop_df, household_id == 0)$person_id
track_list = list(person_IDs = person_IDs, household_IDs = 0)
write_json(track_list, "track.json", pretty = T)

## add asyptomatic
set.seed(1234)
pop_df$asymptomatic <- runif(nrow(pop_df)) < 0.1
pop_df$stays_home   <- runif(nrow(pop_df)) < 0.8
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

Rcpp::sourceCpp("get_timeseries.cpp")
set.seed(123) # so certain random things always flip the same way

# set some person IDS to track
# and some hh ids

# what do you want to track
track <- jsonlite::read_json("track.json", simplifyVector = T)

out <- get_timeseries(
  df_mat,
  ta_mat,
  hh_V     = 10,  # m^3
  school_V = 100, # m^3
  work_V   = 100, # m^3
  comm_V   = 100, # m^3
  n_days = as.integer(3),
  personIDs_to_track = as.integer(track$person_IDs),
  hhIDs_to_track = as.integer(track$household_IDs)
)

conc = out$hh
conc
hhdt = data.table(conc, hour = 0:(length(conc) - 1))

library(ggplot2)
library(ggpubr)

ggplot(hhdt) + theme_classic2() +
  geom_line(aes(x = hour, y = conc), color = 'blue') +
  ggtitle("household RSV concentration")

person_conc = data.table(out$person_c)
names(person_conc) = as.character(track$person_IDs)
person_conc$hour = 1:nrow(person_conc)
head(person_conc)
person_conc_melt = melt(person_conc, id.vars = "hour")

ggplot(person_conc_melt) + theme_classic2() +
  geom_line(aes(x = hour, y = value, color = variable)) +
  ggtitle("person RSV concentration")

