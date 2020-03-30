# BAN430 - FORECASTING - 2nd Project

library(fpp2)
library(ggplot2)
library(PerformanceAnalytics)
library(corrplot)
library(ggpubr)
library(seasonal)
library(splines)
library(dplyr)
library(magrittr)
library(readr)
library(skimr)

# Loading data ----
# Loading .csv file containing the important data points on "Net national disposable income" (di) and "Final consumption expenditure" (ce)
# for all the countries available. Filtering out unnecessary columns, and changing data format to a wide format
df <- read_csv("data.csv")

df %<>%
  filter(LOCATION == "AUS") %>% 
  select(SUBJECT, TIME, Value) %>%
  tidyr::spread(SUBJECT, Value) %>% # only calling single function to reduce conflicts between tidyr and dplyr
  rename(Date = TIME,
         di = B6NS1,
         ce = P3S1)

# creating ts object
ts <- ts(df[,2:3], start = c(1959,3), end = c(2019,4), frequency = 4)

# binding date information from ts object to original df (easier than transforming the original format to a R date format)
df <- cbind(as.double(time(ts)), df) 
df %<>% select(-Date) %>% 
  rename(Date = "as.double(time(ts))")


