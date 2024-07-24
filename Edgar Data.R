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
library(edgar)
library(tidyedgar) #clean up edgar package : https://github.com/gerardgimenezadsuar/tidyedgar 
library(lubridate) # will parse datestamps into "quarters" using quarter fct

###### DATA ANALYSIS #######

# Import all edgar data from 'edgar' package using 'tiny edgar'
df <- yearly_data(years = 2015:2023)
df_new <- subset(df, df$data.entityName=="APPLE INC.")


