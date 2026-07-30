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
# huh, they have both a work ID and school ID, thats .... incorrect ... ?
# also seems like
hh_ids <- c(0, 1)
subset(pop_df, household_id %in% hh_ids)
person_IDs = subset(pop_df, household_id %in% hh_ids)$person_id
work_IDs = subset(pop_df, household_id %in% hh_ids)$work_id
school_IDs = subset(pop_df, household_id %in% hh_ids)$school_id
comm_IDs = subset(pop_df, household_id %in% hh_ids)$community_id

track_list = list(person_IDs = person_IDs,
                  household_IDs = hh_ids,
                  work_IDs = work_IDs,
                  school_IDs = school_IDs,
                  comm_IDs = comm_IDs)

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
head(df_mat)

ta_mat <- as.matrix(time_activity)
mode(ta_mat) <- 'integer'
head(ta_mat)

Rcpp::sourceCpp("get_timeseries.cpp")
set.seed(123) # so certain random things always flip the same way

# set some person IDS to track
# and some hh ids

# what do you want to track
track <- jsonlite::read_json("track.json", simplifyVector = T)

n_days = 30

LOCAL = T

if(LOCAL) {

  out <- get_timeseries(
    df_mat,
    ta_mat,
    hh_V     = 10,  # m^3
    school_V = 100, # m^3
    work_V   = 100, # m^3
    comm_V   = 100, # m^3
    n_days = as.integer(n_days),
    personIDs_to_track = as.integer(track$person_IDs),
    hhIDs_to_track = as.integer(track$household_IDs)
  )

  make_plots <- function(out) {



    library(ggplot2)
    library(ggpubr)
    library(patchwork)

    get_rect_hours <- function(starttime, n_days) {
      xx <- rep(24, n_days)
      yy <- 0:(n_days - 1)
      zz <- xx * yy
      o <- rep(starttime, n_days)
      return(o + zz)
    }

    make_bars <- function(starttime, endtime, xfill) {
      return(annotate(geom = 'rect',
               xmin = get_rect_hours(starttime, n_days),
               xmax = get_rect_hours(endtime, n_days),
               ymin= -Inf, ymax = Inf, fill = xfill))
    }

    ## if track has household ID then plot this
    hh_conc = data.table(out$hh)
    names(hh_conc) = as.character(track$household_IDs)
    hh_conc$hour = 1:nrow(hh_conc)
    head(hh_conc)
    hh_conc_melt = melt(hh_conc, id.vars = "hour")

    p1 <- ggplot(hh_conc_melt) + theme_classic2() +
      make_bars(-4, 8, 'grey95') +
      make_bars(8, 9, 'lightyellow') +
      make_bars(9, 17, 'lavender') +
      make_bars(17, 20, 'lightyellow') +
      geom_line(aes(x = hour, y = value, color = variable)) +
      ggtitle("household RSV concentration [mass/volume]")

    person_conc = data.table(out$person_c)
    names(person_conc) = as.character(track$person_IDs)
    person_conc$hour = 1:nrow(person_conc)
    head(person_conc)
    person_conc_melt = melt(person_conc, id.vars = "hour")

    ## if track has person ID then plot this
    p2 <- ggplot(person_conc_melt) + theme_classic2() +
      make_bars(-4, 8, 'grey95') +
      make_bars(8, 9, 'lightyellow') +
      make_bars(9, 17, 'lavender') +
      make_bars(17, 20, 'lightyellow') +
      geom_line(aes(x = hour, y = value, color = variable)) +
      ggtitle("person RSV internal mass [mass]")

    person_seir = data.table(out$person_seir)
    names(person_seir) = as.character(track$person_IDs)
    person_seir$hour = 1:nrow(person_seir)
    head(person_seir)
    tail(person_seir)
    person_seir_melt = melt(person_seir, id.vars = "hour")

    p3 <- ggplot(person_seir_melt) + theme_classic2() +
      make_bars(-4, 8, 'grey95') +
      make_bars(8, 9, 'lightyellow') +
      make_bars(9, 17, 'lavender') +
      make_bars(17, 20, 'lightyellow') +
      geom_line(aes(x = hour, y = value, color = variable)) +
      ggtitle("person SEIR")

    p1 / p2 / p3

  }

  make_plots(out)

}

