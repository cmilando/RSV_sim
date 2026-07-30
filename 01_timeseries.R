library(data.table)
library(jsonlite)
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

get_melt <- function(x, xnames, xcol = 'hour', xoffset = 0) {
  x = data.table(x)
  names(x) = as.character(xnames)
  x$new_col_name_here = (1:nrow(x)) - xoffset
  names(x)[ncol(x)] = xcol
  return(melt(x, id.vars = xcol))
}

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
hh_ids <- c(0, 1, 5)
subset(pop_df, household_id %in% hh_ids)
person_IDs = subset(pop_df, household_id %in% hh_ids)$person_id
work_IDs = subset(pop_df, household_id %in% hh_ids & work_id != -999)$work_id
school_IDs = subset(pop_df, household_id %in% hh_ids & school_id != -999)$school_id
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

## add stays home when infected
pop_df$stays_home   <- runif(nrow(pop_df)) < 0.8
head(pop_df)

## and seed the initial infections
pop_df$infected <- FALSE
# rr <- runif(nrow(pop_df)) < 0.05
rr <- pop_df$community_id == 1
pop_df$infected[rr] <- TRUE
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
track

n_days = 50

LOCAL = T

if(LOCAL) {

  make_tracked_plots <- function(out) {

    ## if track has household ID then plot this
    hh_conc_melt <- get_melt(out$household_conc, track$household_IDs)

    p1 <- ggplot(hh_conc_melt) + theme_classic2() +
      make_bars(-4, 8, 'grey95') +
      make_bars(8, 9, 'lightyellow') +
      make_bars(9, 17, 'lavender') +
      make_bars(17, 20, 'lightyellow') +
      geom_line(aes(x = hour, y = value, color = variable),
                show.legend = F) +
      ggtitle("household RSV concentration [mass/volume]")

    ## if track has person ID then plot this
    person_conc_melt = get_melt(out$person_mass, track$person_IDs)

    p2 <- ggplot(person_conc_melt) + theme_classic2() +
      make_bars(-4, 8, 'grey95') +
      make_bars(8, 9, 'lightyellow') +
      make_bars(9, 17, 'lavender') +
      make_bars(17, 20, 'lightyellow') +
      geom_line(aes(x = hour, y = value, color = variable),
                show.legend = F) +
      ggtitle("person RSV internal mass [mass]")

    person_seir_melt = get_melt(out$person_seir, track$person_IDs)

    p3 <- ggplot(person_seir_melt) + theme_classic2() +
      make_bars(-4, 8, 'grey95') +
      make_bars(8, 9, 'lightyellow') +
      make_bars(9, 17, 'lavender') +
      make_bars(17, 20, 'lightyellow') +
      geom_line(aes(x = hour, y = value, color = variable),
                show.legend = F) +
      ggtitle("person SEIR")

    #
    work_melt <- get_melt(out$work_conc, track$work_IDs)
    p4 <- ggplot(work_melt) + theme_classic2() +
      make_bars(-4, 8, 'grey95') +
      make_bars(8, 9, 'lightyellow') +
      make_bars(9, 17, 'lavender') +
      make_bars(17, 20, 'lightyellow') +
      geom_line(aes(x = hour, y = value, color = variable),
                show.legend = F) +
      ggtitle("work RSV concentration [mass/volume]")

    #
    school_melt <- get_melt(out$school_conc, track$school_IDs)
    p5 <- ggplot(school_melt) + theme_classic2() +
      make_bars(-4, 8, 'grey95') +
      make_bars(8, 9, 'lightyellow') +
      make_bars(9, 17, 'lavender') +
      make_bars(17, 20, 'lightyellow') +
      geom_line(aes(x = hour, y = value, color = variable),
                show.legend = F) +
      ggtitle("school RSV concentration [mass/volume]")

    #
    community_melt <- get_melt(out$community_conc, track$comm_IDs)
    p6 <- ggplot(community_melt) + theme_classic2() +
      make_bars(-4, 8, 'grey95') +
      make_bars(8, 9, 'lightyellow') +
      make_bars(9, 17, 'lavender') +
      make_bars(17, 20, 'lightyellow') +
      geom_line(aes(x = hour, y = value, color = variable),
                show.legend = F) +
      ggtitle("community RSV concentration [mass/volume]")

    #
    p2 + p3 + p1 + p4 + p5 + p6 +
      plot_layout(ncol = 2)

  }
  make_diagnostic_plots <- function(out) {

    ## if track has household ID then plot this
    incidence_melt <- get_melt(out$incidence, 0:100)
    incidence_melt$age <- as.integer(as.character(incidence_melt$variable))

    p1 <- ggplot(incidence_melt) + theme_classic2() +
      geom_tile(aes(x = hour, y = age, fill = value)) +
      scale_fill_gradientn(colors = c('white', 'red', 'purple')) +
      ggtitle("Incidence (new cases each hour)")

    prevalence_melt <- get_melt(out$prevalence, 0:100)
    prevalence_melt$age <- as.integer(as.character(prevalence_melt$variable))

    p2 <- ggplot(prevalence_melt) + theme_classic2() +
      geom_tile(aes(x = hour, y = age, fill = value)) +
      scale_fill_gradientn(colors = c('white', 'red', 'purple')) +
      ggtitle("Prevalence (total cases each hour)")

    location_vec <- c('Household', 'Work', 'School', 'Community')
    incid_location_melt <- get_melt(out$incidence_location, location_vec,
                                    xcol = 'age', xoffset = 1)
    rr <- which(incid_location_melt$value == 0)
    incid_location_melt$value[rr] <- NA
    names(incid_location_melt)[2] = 'location'

    p3 <- ggplot(incid_location_melt) + theme_classic2() +
      geom_tile(aes(x = location, y = age, fill = value)) +
      ggtitle("Incidence location") +
      scale_fill_viridis_c(option = "magma")

    #
    p1 + p2 + p3 +
      plot_layout(ncol = 3)

  }

  out <- get_timeseries(
    df_mat,
    ta_mat,
    hh_V     = 10,  # m^3
    school_V = 100, # m^3
    work_V   = 100, # m^3
    comm_V   = 100, # m^3
    n_days = as.integer(n_days),
    personIDs_to_track = as.integer(track$person_IDs),
    hhIDs_to_track = as.integer(track$household_IDs),
    workIDs_to_track = as.integer(track$work_IDs),
    schoolIDs_to_track = as.integer(track$school_IDs),
    commIDs_to_track = as.integer(track$comm_IDs)
  )

  make_tracked_plots(out)
  make_diagnostic_plots(out)

}

