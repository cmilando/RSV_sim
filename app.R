library(shiny)
library(jsonlite)
library(data.table)
library(ggplot2)
library(ggpubr)

#----------------------------------------------------------
# Helper plotting functions
#----------------------------------------------------------

source("01_timeseries.R")

get_rect_hours <- function(starttime, n_days) {
  starttime + 24 * (0:(n_days - 1))
}

make_bars <- function(starttime, endtime, fill) {
  annotate(
    "rect",
    xmin = get_rect_hours(starttime, n_days),
    xmax = get_rect_hours(endtime, n_days),
    ymin = -Inf,
    ymax = Inf,
    fill = fill
  )
}

#----------------------------------------------------------
# UI
#----------------------------------------------------------

ui <- fluidPage(

  # max width
  style = "max-width: 700px;",

  titlePanel("RSV Tracking"),

  actionButton("run", "Run model"),

  br(),
  br(),

  uiOutput("tabs")
)

#----------------------------------------------------------
# SERVER
#----------------------------------------------------------

server <- function(input, output, session){

  results <- eventReactive(input$run, {

    track <- read_json("track.json", simplifyVector = TRUE)

    out <- get_timeseries(
      df_mat,
      ta_mat,
      hh_V     = 10,
      school_V = 100,
      work_V   = 100,
      comm_V   = 100,
      n_days = as.integer(n_days),
      personIDs_to_track = as.integer(track$person_IDs),
      hhIDs_to_track = as.integer(track$household_IDs)
    )

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

    tr <- results()$track

    tabs <- list()

    if (length(tr$household_IDs) > 0) {
      tabs[[length(tabs) + 1]] <- tabPanel(
        "Household",
        plotOutput("householdPlot")
      )
    }

    if (length(tr$person_IDs) > 0) {
      tabs[[length(tabs) + 1]] <- tabPanel(
        "Person concentration",
        plotOutput("personConcPlot")
      )

      tabs[[length(tabs) + 1]] <- tabPanel(
        "Person SEIR",
        plotOutput("personSEIRPlot")
      )
    }

    do.call(tabsetPanel, tabs)

  })

  #--------------------------------------------------------
  # Household plot
  #--------------------------------------------------------

  output$householdPlot <- renderPlot({

    req(results())

    conc <- results()$out$hh

    hhdt <- data.table(
      hour = 0:(length(conc)-1),
      conc = conc
    )

    ggplot(hhdt) +
      theme_classic2() +
      make_bars(-4,8,"grey95") +
      make_bars(8,9,"lightyellow") +
      make_bars(9,17,"lavender") +
      make_bars(17,20,"lightyellow") +
      geom_line(aes(hour, conc), colour="blue") +
      ggtitle("Household RSV concentration")

  }, res = 92)

  #--------------------------------------------------------
  # Person concentration
  #--------------------------------------------------------

  output$personConcPlot <- renderPlot({

    req(results())

    track <- results()$track

    person_conc <- data.table(results()$out$person_c)
    names(person_conc) <- as.character(track$person_IDs)

    person_conc$hour <- seq_len(nrow(person_conc))

    person_conc <- melt(person_conc, id.vars="hour")

    ggplot(person_conc) +
      theme_classic2() +
      make_bars(-4,8,"grey95") +
      make_bars(8,9,"lightyellow") +
      make_bars(9,17,"lavender") +
      make_bars(17,20,"lightyellow") +
      geom_line(aes(hour, value, colour=variable)) +
      ggtitle("Person RSV concentration")

  }, res = 92)

  #--------------------------------------------------------
  # Person SEIR
  #--------------------------------------------------------

  output$personSEIRPlot <- renderPlot({

    req(results())

    track <- results()$track

    person_seir <- data.table(results()$out$person_seir)
    names(person_seir) <- as.character(track$person_IDs)

    person_seir$hour <- seq_len(nrow(person_seir))

    person_seir <- melt(person_seir, id.vars="hour")

    ggplot(person_seir) +
      theme_classic2() +
      make_bars(-4,8,"grey95") +
      make_bars(8,9,"lightyellow") +
      make_bars(9,17,"lavender") +
      make_bars(17,20,"lightyellow") +
      geom_line(aes(hour, value, colour=variable)) +
      ggtitle("Person SEIR")

  }, res = 92)

}

shinyApp(ui, server)
