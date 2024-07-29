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
library(edgar) # SEC data and filings from company
library(tidyedgar) #clean up edgar package : https://github.com/gerardgimenezadsuar/tidyedgar 
library(lubridate) # will parse datestamps into "quarters" using quarter fct
library(fredr) # FRED data 

# Import all edgar data from 'edgar' package using 'tiny edgar'
# Takes awhile -- 30-60 seconds -- so need to move to an offline operation
# that stores a db locally or on a server
# Can make as a reactive element in Server but should download database during inital
# boot up and then store locally the data frame to manipulate
df <- yearly_data(years = 2022:2023)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(title=h4("Edgar Info", align="left")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        # Select Company 
        selectInput(inputId = "companies", 
                    label = "Choose Companies", 
                    multiple = TRUE,
                    choices = unique(df$data.entityName)),
        
        # Select Metric / KPI 
        selectInput(inputId = "metrics", 
                    label = "Choose Metric", 
                    multiple = FALSE,
                    choices = c("Gross Profit", 
                                "Operating Income",
                                "% Change in Operating Income",
                                "Revenue",
                                "% Change in Revenue",
                                "Net Income",
                                "% Change in Net Income",
                                "Operating Margin",
                                "Gross Margin",
                                "Net Margin")),
        
        # Date Range for Analysis
        dateRangeInput("daterange1", "Date range:",
                       start = Sys.Date()-360,
                       end = Sys.Date())
        
        
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("companiesSEL")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$companiesSEL <- renderText({ 
    length(input$companies)
    })
  
  # Now using length, make the subset of df based on number of companies selected
  # so a for loop with df_new <- subset(df,df$entityname == input$companies[ii])
  NNEEEDEDED TO DO
  
  # # subset the data frame to the companies
  # for(ii in 1:length(input$companies)){
  #   
  # }

  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
