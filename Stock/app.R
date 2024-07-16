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
    textInput(inputId="stockID", label="Pick a Stock", value = "AAPL", 
            width = NULL, placeholder = NULL),
  
    # Date Range for Stock
    dateRangeInput("daterange1", "Date range:",
                 start = Sys.Date()-30,
                 end = Sys.Date())
  ),
  
  mainPanel(
    
    #Plot Graph
    plotOutput("plot1")
    
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
  
  # Add in column for percent change from nominal
  # db$percent_adj_movement <- (db$cumret_adjusted_prices-1)*100
  
  # Once figured out above then need to add Moving Averages
  # db <- na.omit(db)
  # db$MA_50 <- SMA(db$price_close, n = 50)
  # db$MA_200 <- SMA(db$price_close, n = 200)
  
  ###### GRAPHS #######

  # Plot of the closing price of stock
    output$plot1 <- renderPlot({
      ggplot(db(), aes(x=ref_date, y=price_close)) +
        geom_line() +
        theme_bw() +
        xlab('Date') + ylab('Closing Price [$]') +
        ggtitle('Stock Performance Over Period of Time') +
        theme(plot.title = element_text(hjust = 0.5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
