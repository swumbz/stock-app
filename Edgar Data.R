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
# Takes awhile -- 30-60 seconds
df <- yearly_data(years = 2015:2023)
# old school dataframe subsetting
df_old <- subset(df, df$data.entityName=="APPLE INC.")
# new school subsetting with dplyr and tibbles -- 
# ^ starts with and (?i) any capitalization
# subsetting the comapnies below
df_new <- df %>% 
  filter(data.entityName %>% str_detect("^(?i)apple inc") | 
           data.entityName %>% str_detect("^(?i)microso") | 
           data.entityName %>% str_detect("(?i)alphabet inc") | 
           data.entityName %>% str_detect("(?i)tesla")
         ) 

# Clean up by removing n/a and making sure Dates are Dates
# df_new <- na.omit(df_new)
df_new$data.end <- as.Date(df_new$data.end)
df_new$data.start <- as.Date(df_new$data.start)

# Plot
p2 <- ggplot(df_new, aes(x=data.end,y=operating_margin*100, colour = data.entityName)) +
  geom_line() +
  theme_light()
p2



# Only revenue data
revenue1 <- get_ydata(account = "Revenues")
revenue_new <- subset(revenue1, revenue1$data.cik=="320193") # Apple CIK

####### FRED Data for Interest Rate ########
popular_funds_series <- fredr_series_search_text(
  search_text = "federal funds",
  order_by = "popularity",
  sort_order = "desc",
  limit = 1
)

popular_funds_series_id <- popular_funds_series$id

popular_funds_series_id %>%
  fredr(
    observation_start = as.Date("1990-01-01"),
    observation_end = as.Date("2024-07-01")
  ) %>%
 
   ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() +
  labs(x = "Observation Date", y = "Rate", color = "Series") +
  theme_light()

####### FRED Data for ########
