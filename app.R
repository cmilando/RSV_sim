library(shiny)
library(jsonlite)
library(data.table)
library(ggplot2)
library(ggpubr)
library(bslib)

#----------------------------------------------------------
# Helper plotting functions
#----------------------------------------------------------

source("01_timeseries.R")

#----------------------------------------------------------
# UI
#----------------------------------------------------------

ui <- fluidPage(

  # max width
  style = "max-width: 1000px;",

  titlePanel("RSV Tracking"),

  shiny::inputPanel(

    ##
    actionButton("run", "Run model"),

    ##
    numericInput(
      "n_days", "N days", 50, min = 1, max = 100, step = 1
    ),

    ##
    uiOutput("xzoom_ui"),

    ##
    numericInput(
      "transProb", "Transmission probability",
      value = 0.25, min = 0, max = 1
    )

  ),
  br(),
  br(),

  uiOutput("tabs")
)

#----------------------------------------------------------
# SERVER
#----------------------------------------------------------

server <- function(input, output, session){

  output$xzoom_ui <- renderUI({
    sliderInput("xzoom", "X range",
                min = 0, max = input$n_days * 24,
                value = c(0, input$n_days * 12))
  })

  results <- eventReactive(input$run, {

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

    make_tracked_plots(out, xzoom = c(input$xzoom[1], input$xzoom[2]))

  }, res = 92, height = 1600)

  #--------------------------------------------------------
  # diagnosticPlots
  #--------------------------------------------------------

  output$diagnosticPlots <- renderPlot({

    req(results())

    out <- results()$out

    make_diagnostic_plots(out, xzoom = c(input$xzoom[1], input$xzoom[2]))

  }, res = 92, height = 1000)

}

shinyApp(ui, server)
