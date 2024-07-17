rm(list = ls())

###### LIBRARIES AND SETUP #######
library(shiny)

# Plotting Packages
library(ggplot2)
library(ggthemes)

# Data Packages
library(csvread)
library(reshape2)
library(dplyr)
library(tidyverse)

# Financial Packages
library(yfR)
library(quantmod)
library(WeibullR)
library(edgar)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(title=h4("Stocks", align="center")),
  
  sidebarPanel(
  
    # Stock Input
    textInput(inputId="stockID", label="Pick a Stock", value = "NVDA", 
            width = NULL, placeholder = NULL),
  
    # Date Range for Stock
    dateRangeInput("daterange1", "Date range:",
                 start = Sys.Date()-360,
                 end = Sys.Date())
  ),
  
  mainPanel(
    
    #Plot Graph
    plotOutput("plot1"),
    plotOutput("plot2")
    
    )
)
  

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
  
  # Make new variable for SMA50
  MA50 <- reactive({
    SMA(db()$price_close, n = 50)
  })
  
  # Make new variable for SMA200
  MA200 <- reactive({
    SMA(db()$price_close, n = 200)
  })
  
  # Cbind into new data.frame
  db2 <- reactive({
    cbind(db(),percent_adj_movement(),MA50(),MA200())
  })
  
  # Use db3 instead of db2 if you only want the sections with SMA
  db3 <- reactive({
    na.omit(db2())
  })
  

  # Add in column for percent change from nominal
  # db$percent_adj_movement <- (db$cumret_adjusted_prices-1)*100
  ## Could add it in the calculation for AES of Y
  
  # Once figured out above then need to add Moving Averages
  # db <- na.omit(db)
  # db$MA_50 <- SMA(db$price_close, n = 50)
  # db$MA_200 <- SMA(db$price_close, n = 200)
  
  # Another approach using getSymbols
  # getSymbols("AAPL")
  # db <- data.frame(date=index(AAPL), coredata(AAPL))

  
  ###### GRAPHS #######

  # Plot of the percent change of stock
    output$plot1 <- renderPlot({
      ggplot(db2(), aes(x=ref_date, y=db2()$percent_adj_movement)) +
        geom_line() +
        theme_bw() +
        xlab('Date') + ylab('Percent Change over Time [%]') +
        ggtitle('Stock Performance Over Period of Time') +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    # Plot value of stock and moving average
    output$plot2 <- renderPlot({
      
      # need an if statement that if the date range is < 200 then display null 
      # otherwise display entire graph.  Really should set two variables for moving 
      # average and then only display them if they meet the requirements. so 
      # geom_line dependent on if statement
      
      ggplot(db2(), aes(x=ref_date)) +
        geom_line(aes(y=price_close), color = "blue", size = 1) +
        geom_line(aes(y = db2()$MA50), color = "orange", size = 1) +
        geom_line(aes(y = db2()$MA200), color = "red", size = 1) +
        theme_bw() +
        xlab('Date') + ylab('Price of Stock [$]') +
        ggtitle('Stock Performance Over Period of Time') +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_log10()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
