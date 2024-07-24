rm(list = ls())

###### LIBRARIES AND SETUP #######
library(shiny)

# Plotting Packages
library(ggplot2)
library(ggthemes)
library(ggrepel)

# Data Packages
library(csvread)
library(reshape2)
library(dplyr)
library(tidyverse)

# Financial Packages
library(yfR)
library(quantmod)
library(WeibullR)
library(edgar) # SEC data and filings from company
library(tidyedgar) #clean up edgar package : https://github.com/gerardgimenezadsuar/tidyedgar 
library(lubridate) # will parse datestamps into "quarters" using quarter fct
library(fredr) # FRED data

###### APP BELOW #######

###### UI #######
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(title=h4("Stocks", align="left")),
  
  sidebarPanel(
  
    # Stock Input
    textInput(inputId="stockID", label="Pick a Stock", value = "NVDA", 
            width = NULL, placeholder = NULL),
  
    # Date Range for Stock
    dateRangeInput("daterange1", "Date range:",
                 start = Sys.Date()-720,
                 end = Sys.Date()),
    
    # Moving Average Value
    numericInput("SMA1", label="First Moving Average", 
                 value=50, min=5, max = 500),
    
    # Moving Average Value
    numericInput("SMA2", label="Second Moving Average", 
                 value=200, min=5, max = 500),
    
    # Change limits and scale on Plot 2
    numericInput("ymax1", label="Graph Upper Limit", value = NULL),
    numericInput("ymin1", label="Graph Lower Limit", value = NULL),
    selectInput("loglin1", "Y-Axis Setting", c("linear","log"))
    
  ),
  
  mainPanel(
    
    #Plot Graph
    plotOutput("plot2"),
    plotOutput("plot1")
    
    )
)
  
###### SERVER #######
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get the stock data from Yahoo
  db <- reactive({
    yf_get(
      tickers = input$stockID,
      first_date = input$daterange1[1],
      last_date = input$daterange1[2])
    })
  
  # Make new variable for percentage change
  percent_adj_movement <- reactive({
    (db()$cumret_adjusted_prices-1)*100
  })
  
  # New variable checking SMA values to date range
  SMA_check_Date <- reactive({
    as.numeric(input$daterange1[2]-input$daterange1[1])})
  SMA_check_SMA <- reactive({
    as.numeric(min(input$SMA1,input$SMA2))})
  
  # Old school MA50,200
  MA200 <- reactive({
    SMA(db()$price_close, n = input$SMA2)})
  MA50 <- reactive({
    SMA(db()$price_close, n = input$SMA1)})
  
  
  # Make new variable for SMA50 that is only calculated when the date range is
  # greater than moving average number... test 2 failed
  # if (SMA_check_Date() > SMA_check_SMA()) {
  #   MA50 <- reactive({
  #     SMA(db()$price_close, n = input$SMA1)})
  #   MA200 <- reactive({
  #     SMA(db()$price_close, n = input$SMA2)})
  # }else{
  #   MA50 <- NULL
  #   MA200 <- NULL
  # }
  
  # Make new variable for SMA200 that is dependent on date range.... test 1 failed
  # MA200 <- reactive({
  #   if(as.numeric(input$daterange1[2]-input$daterange1[1]) > min(input$SMA1,input$SMA2)){
  #     SMA(db()$price_close, n = input$SMA2)
  #   }else{NULL}
  # })
  
  # Cbind into new data.frame
  db2 <- reactive({
    cbind(db(),percent_adj_movement(),MA50(),MA200())
  })
  
  # Use db3 instead of db2 if you only want the sections with SMA
  db3 <- reactive({
    na.omit(db2())
  })
  
  
  ###### GRAPHS #######

  # Plot of the percent change of stock
    output$plot1 <- renderPlot({
      
      # Need a new column that looks at change over time and if positive then 
      # color line green but if negative then color line red
      
      ggplot(db2(), aes(x=ref_date, y=db2()$percent_adj_movement)) +
        geom_line(color="gray18", size = 1) +
        theme_light() +
        xlab('Date') + ylab('Percent Change over Time [%]') +
        ggtitle('Stock Relative Growth over Time') +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    # Plot value of stock and moving average
    output$plot2 <- renderPlot({
      
      # need an if statement that if the date range is < 200 then display null 
      # otherwise display entire graph.  Really should set two variables for moving 
      # average and then only display them if they meet the requirements. so 
      # geom_line dependent on if statement
      if(input$loglin1=="log"){
          ggplot(db2(), aes(x=ref_date)) +
            geom_line(aes(y=price_close), 
                      color = "gray18", 
                      size = 1) +
            geom_line(aes(y = db2()$MA50), 
                      color = "springgreen2", 
                      size = 1) +
            geom_line(aes(y = db2()$MA200), 
                      color = "chocolate2", 
                      size = 1) +
            theme_light() +
            xlab('Date') + ylab('Price of Stock [$]') +
            ggtitle('Stock Performance Over Period of Time') +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_y_log10()
      } else {
        ggplot(db2(), aes(x=ref_date)) +
          geom_line(aes(y=price_close), 
                    color = "gray18", 
                    size = 1) +
          geom_line(aes(y = db2()$MA50), 
                    color = "springgreen2", 
                    size = 1) +
          geom_line(aes(y = db2()$MA200), 
                    color = "chocolate2", 
                    size = 1) +
          theme_light() +
          xlab('Date') + ylab('Price of Stock [$]') +
          ggtitle('Stock Performance Over Period of Time') +
          theme(plot.title = element_text(hjust = 0.5))
      }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
