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

###### DATA ANALYSIS #######

# Import all edgar data from 'edgar' package using 'tiny edgar'
df <- yearly_data(years = 2015:2023)
df_new <- subset(df, df$data.entityName=="APPLE INC.")

# Only revenue data
revenue1 <- get_ydata(account = "Revenues")
revenue_new <- subset(revenue1, revenue1$data.cik=="320193") # Apple CIK

