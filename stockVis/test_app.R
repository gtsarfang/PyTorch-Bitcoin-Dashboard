# Load packages ----
library(shiny)
library(shinydashboard)
library(quantmod)
library(shinythemes)
library(flexdashboard)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- dashboardPage(
  dashboardHeader(title = "Crypto Forecaster"),

  #sidebarLayout(
    
    dashboardSidebar(

      textInput("symb", "Cryptocurrency","GOOG"),
      
      numericInput("value", 
                   label = "Pairs", 
                   min = 0, max = 1, value = 0.5, step = 0.1),
      
      dateRangeInput("dates",
                     "Date range",
                     start = "2013-01-01",
                     end = as.character(Sys.Date())),
  
      br(),
      br(),
  
      checkboxInput("log", "Plot y axis on log scale",
                    value = FALSE)
    ),
  
    dashboardBody(
      fluidRow(
        plotOutput("plot")
      ),
      
      br(),
      
      fluidRow(
        infoBoxOutput("dateBox"),
        infoBoxOutput("speedBox"),
        infoBoxOutput("pulseBox"),
        gaugeOutput("gauge")
      )
    )
  #)
)

# Server logic
server <- function(input, output) {

  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })

  output$plot <- renderPlot({

    chartSeries(dataInput(), theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })

  output$dateBox <- renderInfoBox({
    infoBox(
      "Monthly average", paste(333.33, "USD"), icon = icon("calendar"),
      color = "green", fill=TRUE
    )
  })
  
  output$speedBox <- renderInfoBox({
    infoBox(
      "Weekly average", paste(420.69, "USD"), icon = icon("road"),
      color = "yellow"
    )
  })
  
  output$pulseBox <- renderInfoBox({
    infoBox(
      "Daily average", paste(777.777, "USD"), icon = icon("heart"),
      color = "red"
    )
  })
  
  output$gauge = renderGauge({
    gauge(input$value, 
          min = 0, 
          max = 1, 
          sectors = gaugeSectors(success = c(0.5, 1), 
                                 warning = c(0.3, 0.5),
                                 danger = c(0, 0.3)))
  })
  
}

# Run the app
shinyApp(ui, server)
