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
library(urca)

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

# split into test and train set (10 years of prediction)
ts.train <- ts(df[,2:3], start = c(1959,3), end = c(2009,4), frequency = 4)
ts.test <- ts(df[,2:3], start = c(2010,1), end = c(2019,4), frequency = 4)

# ANALYSIS IF TIME SERIES
skim(df)

di.plot <- autoplot(ts[,1], color = "red") + 
  ggtitle("Net national Disposable Income (di)", subtitle = "Not seasonally adj.: from 1959 to 2020") +
  scale_x_continuous(name = "Years",
                     limits = c(1960, 2020),
                     breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(name = "Disposable Income (in AUD millions)",
                     limits = c(0, 500000),
                     breaks = seq(0, 500000, by = 100000)) +
  theme_minimal()

ce.plot <- autoplot(ts[,2], color = "red") + 
  ggtitle("Final Consumption Expenditure", subtitle = "Not seasonally adj.: from 1959 to 2020") +
  scale_x_continuous(name = "Years",
                     limits = c(1960, 2020),
                     breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(name = "Consumption Exp. (in AUD millions)",
                     limits = c(0, 500000),
                     breaks = seq(0, 500000, by = 100000)) +
  theme_minimal()

ggarrange(di.plot, ce.plot, nrow = 2) # Figure 1

# TIME SERIES PROPERTY ANALYSIS
# Checking the residuals indicates that lots of autocorrelation, in other words: valuable information remains in the residuals,
# that is currently not used to predict the data. This autocorrelation pattern is also typical for this type of economic 
# time series. The significance of this issue is very high, as indicated by the Ljung-Box test. Additionally, strong trend cycles 
# and seasonality visible in the previous, ACF, and residual plots indicate that both series are non-stationary. 
# Based on the distribution of the residuals we also see, that stabilisation of variance and mean are particularly 
# important in order to design a suitable forecasting model based on AR(I)MA.

# For the first characteristic we apply a log-transformation of the underlying data. But, this is not enough, as after the 
# log-transformation not only the autocorrelation and heteroscedasticity issues remain, but also because the distibution
# of the residuals does not have a fitting distribution, that would be based on a standard distribution (Appendix XYZ). 
# For the latter part, we need to work with reasonable differencing methods so that we stabilize the mean and maintain 
# the interpretability of the model and its results. Before we start with ditfferencing, we apply the KPSS test in order
# to see, whether differencing is required:

checkresiduals(ts[,1])
Box.test(ts[,1], lag = 10, type = "Ljung-Box")

ts[,1] %>% ur.kpss() %>% summary()

# Based on this output, we see that the data is non-stationary, the null hypothesis indicating stationary data, can be 
# rejected to all p-levels from 10 to 1 percent. To guess a fitting number of differences we must scrutinise the ACF plots:
# Based on the previous analyses, we know that seasons and trends play a role. A way of account for both issues and to keep
# the interpretability of the results is to apply first-order and seasonal differencing. In this way, we account for quarterly
# and seasonal (so yearly) difference in the series and can interpret the results as _quarterly changes_. 

checkresiduals(log(ts[,1])) # Appendix 1

checkresiduals(diff(diff(log(ts[,1]),4),1)) # Appendix 2

# As can be seen in _Appendix 2_ and after applying log-transformation, seasonal, and first-order differencing, the ACF
# plots looks less impacted by the issues, such as autocorrelation, heteroscedasticity, and in total: non-stationarity. 
# The main and variance are now stabilised. But, there are still many spikes in the ACF and residuals, 
# but those do not follow a specific pattern. Still, the Ljung-Box test indicates that a significant amount of 
# autocorrelation remains in the residuals, which we can not get rid off based on our toolset. Applying the KPSS test,
# we see that the data is now stationary. The difference in both test results is based on autocorrelation or serial correlation
# which is a much stronger indication than simple stationarity. 


Box.test(diff(diff(log(ts[,1]),4),1), lag = 10, type = "Ljung-Box")
diff(diff(log(ts[,1]),4),1) %>% ur.kpss() %>% summary()

# To be completely sure this transformation is correct we apply KPSS functions in order to determine lag values for 
# the differencing. Unsuprisingly, KPSS indicates we should use first-order and seasonal differencing.

ts[,1] %>% log() %>% nsdiffs()
ts[,1] %>% log() %>% diff(lag = 4) %>% ndiffs()

# ------

# Exactly the same test and procedure will be applied to the Final Consumption Expenditure. Because both time series 
# look very similar (see Figure XYZ), it can be inferred that a very similar transformation must be applied. This holds true, as 
# the plots in the Appendix show.

checkresiduals(ts[,2]) # Appendix 3
Box.test(ts[,2], lag = 10, type = "Ljung-Box")

ts[,2] %>% ur.kpss() %>% summary()

checkresiduals(log(ts[,2])) # no print
checkresiduals(diff(diff(log(ts[,2]),4),1)) # Appendix 4

Box.test(diff(diff(log(ts[,2]),4),1), lag = 10, type = "Ljung-Box")
diff(diff(log(ts[,2]),4),1) %>% ur.kpss() %>% summary()

ts[,2] %>% log() %>% nsdiffs()
ts[,2] %>% log() %>% diff(lag = 4) %>% ndiffs()




