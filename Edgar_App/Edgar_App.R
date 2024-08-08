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
df <- yearly_data(years = 2020:2023)
colnames(df) <- c("CIK",
                  "Entity.Name",
                  "CCP",
                  "Year",
                  "Taxonomy",
                  "UOM",
                  "Location",
                  "StartDate",
                  "EndDate",
                  "Operating.Income",
                  "Gross.Profit",
                  "Revenue",
                  "Net.Income",
                  "Net.Income.Change",
                  "Revenue.Change",
                  "Operating.Income.Change",
                  "Gross.Margin",
                  "Operating.Margin",
                  "Net.Margin")

df$EndDate <- as.Date(df$EndDate)
df$StartDate <- as.Date(df$StartDate)

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
                    choices = unique(df$Entity.Name)),
        
        # Select Metric / KPI 
        selectInput(inputId = "metrics", 
                    label = "Choose Metric", 
                    multiple = FALSE,
                    choices = c("Gross.Profit", 
                                "Operating.Income",
                                "Operating.Income.Change",
                                "Revenue",
                                "Revenue.Change",
                                "Net.Income",
                                "Net.Income.Change",
                                "Operating.Margin",
                                "Gross.Margin",
                                "Net.Margin")),
        
        # Date Range for Analysis
        dateRangeInput("daterange1", "Date range:",
                       start = Sys.Date()-360,
                       end = Sys.Date())
        
        
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("companiesSEL"),
          textOutput("companies_subset"),
          plotOutput("plot1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Count how many companies selected
  output$companiesSEL <- renderText({ 
    length(input$companies)
    })
  
  # Subset master df based on all companies selected.  Is Tibble right here?
  df_subset <- reactive({
      df %>% filter(Entity.Name == input$companies)
      #df %>% filter(Entity.Name %>% contain(input$companies))
    })
  
  
  # This should match companiesSEL but doesnt
  output$companies_subset <- renderText({ 
    #length(unique(df_subset()$data.entityName))
    #unique(df_subset()$data.entityName)
    df_subset()$Entity.Name
  })
  
  output$plot1 <- renderPlot({
    
    # Need a new column that looks at change over time and if positive then 
    # color line green but if negative then color line red
    
    plot1_df <- df_subset()[,c(
      "EndDate",
      "Entity.Name",
      input$metrics
      )
      ] # NOT WORKING MAY NEED TO TRY TIBBLE
    
    ggplot(df_subset(), aes(x=EndDate, y=as.data.frame(input$metrics)[,], colour = Entity.Name)) +
      geom_line() +
      theme_light() +
      xlab('Date') + ylab('Metric') +
      ggtitle('Title') +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # # subset the data frame to the companies
  # for(ii in 1:length(input$companies)){
  #   
  # }

  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
