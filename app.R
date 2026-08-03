library(shiny)
library(jsonlite)
library(data.table)
library(ggplot2)
library(ggpubr)
library(bslib)
library(svglite)
library(data.table)
library(jsonlite)
library(ggplot2)
library(ggpubr)
library(patchwork)

#----------------------------------------------------------
# Functions
#----------------------------------------------------------

get_rect_hours <- function(starttime, n_days) {
  xx <- rep(24, n_days)
  yy <- 0:(n_days - 1)
  zz <- xx * yy
  o <- rep(starttime, n_days)
  return(o + zz)
}

make_bars <- function(starttime, endtime, xfill, n_days) {
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

make_tracked_plots <- function(out, track, n_days = n_days,
                               xzoom = c(0, n_days * 24), ncol = 1) {

  ###
  person_seir_melt = get_melt(out$person_seir, track$person_IDs)
  person_seir_melt$value <- factor(
    person_seir_melt$value,
    levels = c(0, 1, 2, 3),
    labels = c('Susceptible', 'Exposed', 'Infected', 'Recovered'))

  p1 <- ggplot(person_seir_melt) + theme_classic2() +
    geom_tile(aes(x = hour, y = variable, fill = value),
              color= 'white', linewidth = 0.05) +
    scale_fill_viridis_d(name = NULL) + ylab("person_id") +
    coord_cartesian(xlim = xzoom) +
    theme(axis.text.y = element_blank()) +
    ggtitle("Person SEIR")

  ###
  person_location_melt = get_melt(out$person_location, track$person_IDs)
  person_location_melt$value <- factor(
    person_location_melt$value,
    levels = c(0, 1, 2, 3),
    labels = c('Household', 'Work', 'School', 'Community'))

  p1a <-  ggplot(person_location_melt) + theme_classic2() +
    geom_tile(aes(x = hour, y = variable, fill = value),
              color= 'white', linewidth = 0.05) +
    scale_fill_viridis_d(option = 'magma', name= NULL) +
    ylab("person_id") +
    coord_cartesian(xlim = xzoom) +
    theme(axis.text.y = element_blank()) +
    ggtitle("Person location")

  ### if track has household ID then plot this
  hh_conc_melt <- get_melt(out$household_conc, track$household_IDs)

  p2 <- ggplot(hh_conc_melt) + theme_classic2() +
    make_bars(-4, 8, 'grey95', n_days) +
    make_bars(8, 9, 'lightyellow', n_days) +
    make_bars(9, 17, 'lavender', n_days) +
    make_bars(17, 20, 'lightyellow', n_days) +
    geom_line(aes(x = hour, y = value * 100, color = variable),
              show.legend = F) +
    coord_cartesian(xlim = xzoom, ylim = c(0, 100)) +
    ylab("%") +
    ggtitle("Households: % of population infected")

  ###
  work_melt <- get_melt(out$work_conc, track$work_IDs)
  p3 <- ggplot(work_melt) + theme_classic2() +
    make_bars(-4, 8, 'grey95', n_days) +
    make_bars(8, 9, 'lightyellow', n_days) +
    make_bars(9, 17, 'lavender', n_days) +
    make_bars(17, 20, 'lightyellow', n_days) +
    coord_cartesian(xlim = xzoom, ylim = c(0, 100)) +
    geom_line(aes(x = hour, y = value * 100, color = variable),
              show.legend = F) +
    ylab("%") +
    ggtitle("Work places: % of population infected")

  ###
  school_melt <- get_melt(out$school_conc, track$school_IDs)
  p4 <- ggplot(school_melt) + theme_classic2() +
    make_bars(-4, 8, 'grey95', n_days) +
    make_bars(8, 9, 'lightyellow', n_days) +
    make_bars(9, 17, 'lavender', n_days) +
    make_bars(17, 20, 'lightyellow', n_days) +
    coord_cartesian(xlim = xzoom, ylim = c(0, 100)) +
    geom_line(aes(x = hour, y = value * 100, color = variable),
              show.legend = F) +
    ylab("%") +
    ggtitle("Schools: % of population infected")

  #
  community_melt <- get_melt(out$community_conc, track$comm_IDs)
  p5 <- ggplot(community_melt) + theme_classic2() +
    make_bars(-4, 8, 'grey95', n_days) +
    make_bars(8, 9, 'lightyellow', n_days) +
    make_bars(9, 17, 'lavender', n_days) +
    make_bars(17, 20, 'lightyellow', n_days) +
    coord_cartesian(xlim = xzoom, ylim = c(0, 100)) +
    geom_line(aes(x = hour, y = value * 100, color = variable),
              show.legend = F) +
    ylab("%") +
    ggtitle("Communities: % of population infected")
  p4

  #
  p1 + p1a + p2 + p3 + p4 + p5 +
    plot_layout(ncol = ncol)
}

make_diagnostic_plots <- function(out, n_days = n_days,
                                  xzoom = c(0, n_days * 24)) {
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

  ##
  p_ever_infected = data.table(p = out$p_ever_infected)
  p_ever_infected$hour = 1:nrow(p_ever_infected)

  p4 <- ggplot(p_ever_infected) + theme_classic2() +
    geom_line(aes(x = hour, y = p * 100), col = 'red') +
    coord_cartesian(ylim = c(0, 100)) + ylab("%") +
    ggtitle("Percent ever infected")

  #
  p1 + p2 + p3 + p4 +
    plot_layout(ncol = 1)

}


#----------------------------------------------------------
# Static loads
#----------------------------------------------------------

Rcpp::sourceCpp("get_timeseries.cpp")

# population
pop_df <- readRDS("demo_pop.RDS")

# time-activity
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

#----------------------------------------------------------
# UI
#----------------------------------------------------------

library(shiny)
library(bslib)

ui <- page_fluid(

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),

  div(
    style = "max-width: 1100px; margin: auto;",

    h2("RSV Tracking"),
    br(),

    layout_sidebar(

      sidebar = card(
        card_header("Model Settings"),

        actionButton(
          "run",
          "Run model",
          class = "btn-primary w-100"
        ),

        numericInput(
          "n_days",
          "Simulation days",
          value = 100,
          min = 1,
          max = 100,
          step = 1
        ),

        uiOutput("xzoom_ui"),

        numericInput(
          "transProb",
          "Transmission probability",
          value = 0.00125,
          min = 0,
          max = 1,
          step = 0.01
        ),

        numericInput(
          "p_initInfect",
          "Percent initially infected",
          value = 1,
          min = 0.1,
          max = 50,
          step = 0.1
        ),

        numericInput(
          "p_asymptomatic",
          "Percent asymptomatic",
          value = 10,
          min = 0.1,
          max = 50,
          step = 0.1,width = "80%"
        ),

        numericInput(
          "p_staysHome",
          "Percent stays home when sick",
          value = 80,
          min = 0.1,
          max = 100,
          step = 0.1
        ),

        numericInput(
          "setSeed",
          "Seed",
          value = 12345,
          min = 1,
          step = 1
        )

      ),

      card(
        card_body(
          uiOutput("tabs")
        )
      )
    )
  )
)

#----------------------------------------------------------
# SERVER
#----------------------------------------------------------

server <- function(input, output, session){

  output$xzoom_ui <- renderUI({
    sliderInput("xzoom", "X range",
                min = 0, max = input$n_days * 24,
                value = c(0, input$n_days * 24))
  })

  results <- eventReactive(input$run, {

    set.seed(input$setSeed)

    # all things that follow from random
    pop_size <- nrow(pop_df)
    pop_df$asymptomatic <- runif(pop_size) < (input$p_asymptomatic / 100)
    pop_df$stays_home   <- runif(pop_size) < (input$p_staysHome / 100)
    pop_df$infected <- FALSE
    rr <- sample(1:pop_size,size = (input$p_initInfect / 100) * pop_size,
                 replace = F)
    pop_df$infected[rr] <- TRUE

    # convert to integer
    df_mat <- as.matrix(pop_df)
    mode(df_mat) <- 'integer'
    ta_mat <- as.matrix(time_activity)
    mode(ta_mat) <- 'integer'

    # things to track
    xx <- subset(pop_df, household_id %in% c(1:5))
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

    # run cpp script
    out <- get_timeseries(
      df_mat,
      ta_mat,
      n_days = as.integer(input$n_days),
      ## ** variables for calibration
      transmission_probability = input$transProb,
      ##
      virus_decay_days = as.integer(3),
      incubation_days = as.integer(3),
      recovery_days = as.integer(3),
      ##
      personIDs_to_track = as.integer(track$person_IDs),
      hhIDs_to_track = as.integer(track$household_IDs),
      workIDs_to_track = as.integer(track$work_IDs),
      schoolIDs_to_track = as.integer(track$school_IDs),
      commIDs_to_track = as.integer(track$comm_IDs)
    )

    # update the list
    list(
      track = track,
      out = out
    )

  })

  #--------------------------------------------------------
  # Dynamically create tabs
  #--------------------------------------------------------

  output$tabs <- renderUI({

    req(results())

    tabs <- list()

    tabs[[length(tabs) + 1]] <- tabPanel(
      "Tracked plots",
      helpText("People in the first 4 Households, and
               associated Household School, Work, and Communities"),
      plotOutput("trackedPlots")
    )

    tabs[[length(tabs) + 1]] <- tabPanel(
      "Diagnostic plots",
      plotOutput("diagnosticPlots")
    )

    do.call(tabsetPanel, tabs)

  })

  #--------------------------------------------------------
  # trackedPlots
  #--------------------------------------------------------

  output$trackedPlots <- renderPlot({

    req(results())

    out <- results()$out
    tr  <- results()$track

    make_tracked_plots(out, tr, n_days = input$n_days,
                       xzoom = c(input$xzoom[1], input$xzoom[2]))

  }, height = 1400)

  #--------------------------------------------------------
  # diagnosticPlots
  #--------------------------------------------------------

  output$diagnosticPlots <- renderPlot({

    req(results())

    out <- results()$out

    make_diagnostic_plots(out, xzoom = c(input$xzoom[1], input$xzoom[2]))

  }, height = 1000)

}

shinyApp(ui, server)
