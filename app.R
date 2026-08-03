library(shiny)
library(jsonlite)
library(data.table)
library(ggplot2)
library(ggpubr)
library(bslib)
library(svglite)

#----------------------------------------------------------
# Helper plotting functions
#----------------------------------------------------------

source("01_timeseries.R")
Rcpp::sourceCpp("get_timeseries.cpp")

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
          "setSeed",
          "Seed",
          value = 12345,
          min = 1,
          step = 1
        ),

        br(),

        actionButton(
          "run",
          "Run model",
          class = "btn-primary w-100"
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

    tabs[[length(tabs) + 1]] <- tabPanel(
      "Tracked plots",
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

    make_tracked_plots(out, n_days = input$n_days,
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
