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
    titlePanel("Stocks"),
    
    # Stock Input
    textInput(inputId="stockID", label="Pick a Stock", value = "AAPL", 
              width = NULL, placeholder = NULL),
    
    # Date Range for Stock
    dateRangeInput("daterange1", "Date range:",
                   start = Sys.Date()-30,
                   end = Sys.Date()
                   ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  db <- reactive({
    yf_get(
      tickers = input$stockID,
      first_date = input$daterange1[1],
      last_date = input$daterange1[2]
      )
  })
  
  db$percent_adj_movement <- (db$cumret_adjusted_prices-1)*100
  
  ###### GRAPHS #######

  # Plot of the closing price of stock
    output$distPlot <- renderPlot({
      ggplot(data=db, aes(x=ref_date)) +
        geom_line(aes(y=percent_adj_movement)) +
        theme_bw() +
        xlab('Date') + ylab('Percent Change [%]') +
        ggtitle('Stock Performance Over Period of Time') +
        theme(plot.title = element_text(hjust = 0.5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
