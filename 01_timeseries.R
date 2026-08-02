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

make_tracked_plots <- function(out, xzoom = c(0, n_days * 24)) {

  ###
  person_seir_melt = get_melt(out$person_seir, track$person_IDs)
  person_seir_melt$value <- factor(
    person_seir_melt$value,
    levels = c(0, 1, 2, 3),
    labels = c('Susceptible', 'Exposed', 'Infected', 'Recovered'))

  p1 <- ggplot(person_seir_melt) + theme_classic2() +
    geom_tile(aes(x = hour, y = variable, fill = value),
              color= 'white', linewidth = 0.05) +
    scale_fill_viridis_d() + ylab("person_id") +
    coord_cartesian(xlim = xzoom) +
    theme(axis.text.y = element_blank()) +
    ggtitle("person SEIR")

  ###
  person_location_melt = get_melt(out$person_location, track$person_IDs)
  person_location_melt$value <- factor(
    person_location_melt$value,
    levels = c(0, 1, 2, 3),
    labels = c('Household', 'Work', 'School', 'Community'))

  p1a <-  ggplot(person_location_melt) + theme_classic2() +
    geom_tile(aes(x = hour, y = variable, fill = value),
              color= 'white', linewidth = 0.05) +
    scale_fill_viridis_d(option = 'magma') +
    ylab("person_id") +
    coord_cartesian(xlim = xzoom) +
    theme(axis.text.y = element_blank()) +
    ggtitle("person location")

  ### if track has household ID then plot this
  hh_conc_melt <- get_melt(out$household_conc, track$household_IDs)

  p2 <- ggplot(hh_conc_melt) + theme_classic2() +
    make_bars(-4, 8, 'grey95') +
    make_bars(8, 9, 'lightyellow') +
    make_bars(9, 17, 'lavender') +
    make_bars(17, 20, 'lightyellow') +
    geom_line(aes(x = hour, y = value, color = variable),
              show.legend = F) +
    coord_cartesian(xlim = xzoom) +
    ggtitle("household RSV infected status")

  ###
  work_melt <- get_melt(out$work_conc, track$work_IDs)
  p3 <- ggplot(work_melt) + theme_classic2() +
    make_bars(-4, 8, 'grey95') +
    make_bars(8, 9, 'lightyellow') +
    make_bars(9, 17, 'lavender') +
    make_bars(17, 20, 'lightyellow') +
    coord_cartesian(xlim = xzoom) +
    geom_line(aes(x = hour, y = value, color = variable),
              show.legend = F) +
    ggtitle("work RSV infected status")

  ###
  school_melt <- get_melt(out$school_conc, track$school_IDs)
  p4 <- ggplot(school_melt) + theme_classic2() +
    make_bars(-4, 8, 'grey95') +
    make_bars(8, 9, 'lightyellow') +
    make_bars(9, 17, 'lavender') +
    make_bars(17, 20, 'lightyellow') +
    coord_cartesian(xlim = xzoom) +
    geom_line(aes(x = hour, y = value, color = variable),
              show.legend = F) +
    ggtitle("school RSV infected status")

  #
  community_melt <- get_melt(out$community_conc, track$comm_IDs)
  p5 <- ggplot(community_melt) + theme_classic2() +
    make_bars(-4, 8, 'grey95') +
    make_bars(8, 9, 'lightyellow') +
    make_bars(9, 17, 'lavender') +
    make_bars(17, 20, 'lightyellow') +
    coord_cartesian(xlim = xzoom) +
    geom_line(aes(x = hour, y = value, color = variable),
              show.legend = F) +
    ggtitle("community RSV infected status")
  p4

  #
  p1 + p1a + p2 + p3 + p4 + p5 +
    plot_layout(ncol = 1)
}

make_diagnostic_plots <- function(out, xzoom = c(0, n_days * 24)) {
  ## if track has household ID then plot this
  incidence_melt <- get_melt(out$incidence, 0:100)
  incidence_melt$age <- as.integer(as.character(incidence_melt$variable))

  p1 <- ggplot(incidence_melt) + theme_classic2() +
    geom_tile(aes(x = hour, y = age, fill = value)) +
    scale_fill_gradientn(colors = c('white', 'red', 'purple')) +
    coord_cartesian(xlim = xzoom) +
    ggtitle("Incidence (new cases each hour)")

  prevalence_melt <- get_melt(out$prevalence, 0:100)
  prevalence_melt$age <- as.integer(as.character(prevalence_melt$variable))

  p2 <- ggplot(prevalence_melt) + theme_classic2() +
    geom_tile(aes(x = hour, y = age, fill = value)) +
    scale_fill_gradientn(colors = c('white', 'red', 'purple')) +
    coord_cartesian(xlim = xzoom) +
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
    plot_layout(ncol = 1)

}


#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' get timeseries
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

pop_df <- readRDS("demo_pop.RDS")
head(pop_df)
dim(pop_df)

## add asyptomatic
set.seed(1234)
pop_df$asymptomatic <- runif(nrow(pop_df)) < 0.1

## add stays home when infected
pop_df$stays_home   <- runif(nrow(pop_df)) < 0.8
head(pop_df)

## and seed the initial infections
pop_df$infected <- FALSE
# rr <- runif(nrow(pop_df)) < 0.05
rr <- pop_df$household_id == 3
pop_df$infected[rr] <- TRUE
head(pop_df)

time_activity = data.table(
  hour = 0:23,
  work_location =
    c(0, 0, 0, 0, 0, 0, 0, 0,
      3, 1, 1, 1, 1, 1, 1, 1,
      1, 3, 3, 3, 0, 0, 0, 0),
  school_location =
    c(0, 0, 0, 0, 0, 0, 0, 0,
      3, 2, 2, 2, 2, 2, 2, 2,
      2, 3, 3, 3, 0, 0, 0, 0),
  comm_location =
    c(0, 0, 0, 0, 0, 0, 0, 0,
      3, 3, 3, 0, 0, 0, 0, 0,
      0, 3, 3, 3, 0, 0, 0, 0))

df_mat <- as.matrix(pop_df)
mode(df_mat) <- 'integer'
head(df_mat)

ta_mat <- as.matrix(time_activity)
mode(ta_mat) <- 'integer'
head(ta_mat)
ta_mat


set.seed(123) # so certain random things always flip the same way

# set some person IDS to track
# and some hh ids

# what do you want to track
# set up some things to track
# huh, they have both a work ID and school ID, thats .... incorrect ... ?
# also seems like
xx <- subset(pop_df, household_id %in% c(1:5));
xx
person_IDs = xx$person_id
work_IDs = unique(subset(xx, work_id != -999)$work_id)
school_IDs = unique(subset(xx, school_id != -999)$school_id)
comm_IDs = unique(xx$community_id)
hh_ids = unique(xx$household_id)

track = list(person_IDs = person_IDs,
                  household_IDs = hh_ids,
                  work_IDs = work_IDs,
                  school_IDs = school_IDs,
                  comm_IDs = comm_IDs)

# write_json(track_list, "track.json", pretty = T)
#
# track <- jsonlite::read_json("track.json", simplifyVector = T)
track













