# BAN430 - FORECASTING - 2nd Project
# Libraries and theme----
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
library(tsDyn)
library(ForecastComb)

theme_set(theme_minimal())

# 1. Loading data ----
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

# creating time-series object
tse <- ts(df[,2:3], start = c(1959,3), end = c(2019,4), frequency = 4)

# binding date information from ts object to original df (easier than transforming the original format to a R date format)
df <- cbind(as.double(time(tse)), df) 

df %<>% select(-Date) %>% 
  rename(Date = "as.double(time(tse))")

# 2. Split into test and train set (10 years of prediction, 39 quarters) ----
ts.train <- window (tse,
               start = c(1959,3),
               end = c(2009,4),
               frequency = 4)

ts.test <- window (tse, 
              start = c(2010,1),
              frequency = 4)

# 3. Time-series analysis and summaries ----

# Having a look at the time series, we see a high amount of correlation between both series that needs
# to be analysed using the Augmented Dickey-Fuller (ADF) Test and corrected using Differencing and other transformations.
# We also observe that the seasonality and trend components are similar for both series. Distance between Consumption and 
# Income increases with the later years the distance between the series, meaning that people in Australia earn more than 
# they spend. Also, false regression issues can be discarded as a potential issue, as disposable available income
# is impacts the consumption of the population because consumption depends on earnings and debt available. 

# Analysis of the disposable income ----
full.plot <- autoplot(tse[,1], color = "black", series = "asdasd") +
  autolayer(tse[,2], color = "darkblue", series = "dddd") +
  ggtitle("Net National Disposable Income and Final Consumption Expenditure", 
          subtitle = "Two time series show a high degree of correlation") +
  scale_x_continuous(name = "Years",
                     limits = c(1960, 2020),
                     breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(name = "Disposable Income (in AUD millions)",
                     limits = c(0, 500000),
                     breaks = seq(0, 500000, by = 125000))

zoom.plot <- autoplot(tse[,1], color = "black", series = "asdasd") +
  autolayer(tse[,2], color = "darkblue", series = "dddd") +
  ggtitle("Net National Disposable Income and Final Consumption Expenditure", 
          subtitle = "High degree of correlation even clearer for trend and cycles after zoom-in") +
  scale_x_continuous(name = "Years",
                     limits = c(1990, 2020),
                     breaks = seq(1990, 2020, by = 10)) +
  scale_y_continuous(name = "Consumption Exp. (in AUD millions)",
                     breaks = seq(0, 500000, by = 125000))

ggarrange(full.plot, zoom.plot, nrow = 2) # Figure 1

# Checking the residuals indicates that lots of autocorrelation remains in the residuals. In other words: 
# valuable information that is currently not used to predict the data. This autocorrelation pattern is also 
# typical for this type of economic data and time series. The significance of autocorrelation issue is very high, 
# as indicated by the Ljung-Box test. Additionally, strong trend cycles and seasonality, visible in the ACF and 
# residual plots indicate that both series are non-stationary.
# Based on the distribution of the residuals we also see, that stabilisation of variance and mean are particularly 
# important in order to design a suitable forecasting model based on AR(I)MA.

# To deal with the economic characteristic we apply a log-transformation to the data. But, this is not enough, as after the 
# log-transformation not only the autocorrelation and heteroscedasticity issues remain, but also because the distibution
# of the residuals does not have a fitting Gauss distribution, as can be seen in _Figure 2_. To fix this, we apply differencing methods to stabilize 
# the mean and maintain the interpretability of the model and its results. Before we start with ditfferencing, 
# we apply the KPSS test in order to see, whether differencing is required:

checkresiduals(tse[,1]) # Figure 2
Box.test(tse[,1], lag = 10, type = "Ljung-Box") # Appendix 1

tse[,1] %>% ur.kpss() %>% summary()

# Based on this output, we see that the data is non-stationary, the null hypothesis indicating stationary data, can be 
# rejected to all p-levels from 10 to 1 percent. To guess a fitting number of differences we must scrutinise the ACF plots:
# Based on the previous analyses, we know that seasons and trends play a role. A way to account for both issues and to keep
# the interpretability of the results is to apply first-order and seasonal differencing. In this way, we account for quarterly
# and seasonal (so yearly) difference in the series and can interpret the results as _quarterly changes_. 

checkresiduals(log(tse[,1])) # Appendix 2
checkresiduals(diff(diff(log(tse[,1]),4),1)) # Appendix 3

# As can be seen in _Appendix 2_ and after applying log-transformation, seasonal, and first-order differencing, the ACF
# plots looks less impacted by issues, such as autocorrelation, heteroscedasticity, and in total: non-stationarity. 
# The main and variance are now stabilised. But, there are still many spikes in the ACF and residuals, 
# but those do not follow a specific pattern. Still, the Ljung-Box test indicates that a significant amount of 
# autocorrelation remains in the residuals, which we can not get rid off based on our pre-processing toolset. Specific 
# assumptions and calculations in the ARIMA setting will account for the last issues. Applying the KPSS test,
# we see that the data is now stationary. The difference in both test resultse is based on autocorrelation or serial correlation
# which is a much stronger indication than simple stationarity. 

Box.test(diff(diff(log(tse[,1]),4),1), lag = 10, type = "Ljung-Box")
diff(diff(log(tse[,1]),4),1) %>% ur.kpss() %>% summary()

# To be completely sure this transformation is correct we apply KPSS functions in order to determine lag values for 
# the differencing. Unsuprisingly, KPSS indicates to use first-order and seasonal differencing.

tse[,1] %>% log() %>% nsdiffs()
tse[,1] %>% log() %>% diff(lag = 4) %>% ndiffs()

# Consumption Expenditure Analysis ------

# Exactly the same test and procedure will be applied to the Final Consumption Expenditure. Because both time series 
# look very similar (see _Figure 1_), it can be inferred that a very similar transformation must be applied. This holds true, as 
# the plots Appendix show.

checkresiduals(tse[,2]) # Appendix 4
Box.test(tse[,2], lag = 10, type = "Ljung-Box")

tse[,2] %>% ur.kpss() %>% summary()

checkresiduals(log(tse[,2])) # no print
checkresiduals(diff(diff(log(tse[,2]),4),1)) # Appendix 5

Box.test(diff(diff(log(tse[,2]),4),1), lag = 10, type = "Ljung-Box")
diff(diff(log(tse[,2]),4),1) %>% ur.kpss() %>% summary()

tse[,2] %>% log() %>% nsdiffs()
tse[,2] %>% log() %>% diff(lag = 4) %>% ndiffs()

# we incorporate both log, seasonal and first-order differencing transformed series into the data frame and ts object

df$di.adj <- c(c(rep(NA,5), diff(diff(log(tse[,1]),4),1)))
df$ce.adj <- c(c(rep(NA,5), diff(diff(log(tse[,2]),4),1)))

tse <- ts(df[,2:5], start = c(1959,3), end = c(2019,4), frequency = 4)

# Split again into test and train set, with slightly adjusted start date to account for differencing reduction of the data ----
ts.train <- window(tse,
                   start = c(1960,4),
                   end = c(2009,4),
                   frequency = 4)

ts.test <- window(tse, 
                  start = c(2010,1),
                  end = c(2019,4),
                  frequency = 4)

# We still observe stationarity in our series, but assume that the rest will be resolved employing the ARIMA machanics. 
summary(ur.df(ts.train[,3], type = "none"))
summary(ur.df(ts.train[,4], type = "none"))

# LONG-TERM RELATIONSHIP ----

# 4. Long-term relationship analysis ----

# As already indicated above, just regressing both variables on each other might lead to spurious regressions, identifiable
# by a high Adj. R-sqaured and high residual autocorrelation (Hyndman & Athanasopoulos, 2018). This occurence impacts the 
# reliance of our forecast in the long-term horizon. Based on the previously conducted KPSS test, we already know that 
# non-stationarity and possible cointegration of our series are an issue. These unit root tests already indicated that in
# order to guarantuee that the characteristic equation lies within the unit circle (Hyndman & Athanasopoulos, 2018), we
# must take the differences in order to assure stationarity.

fit.tse <- tslm(formula = ts.train[,1] ~ ts.train[,2]) 
summary(fit.tse)
summary(ur.df(residuals(fit.tse), type = "drift", lag = 1))

fit.tse.adj <- tslm(formula = ts.train[,3] ~ ts.train[,4]) 
summary(fit.tse.adj)
summary(ur.df(residuals(fit.tse.adj), type = "drift", lag = 1))

# As we see, from both analyses, incorporating our transformed series for consumption into the regression 
# does not resolve our non-stationarity issues in the regression setting. But this indicates, 
# that both variables have a significant, cointegrated long-term relationship which can be used to design our forecasts.
# This also holds true if we apply the alternative values for tau2, being -3.43, -2.86, and -2.57.

# 5. Identify ARIMA model for consumption ----

# Firstly, based on the previously determined time series characteristics, we know that we must employ a first-order lag differences 
# for our autoregression (AR) part and a first-order seasonal component for our quarterly data, meaining that d and D are
# equal to one in order to reflect the observations and KPSS test from before. Secondly, we test for an optimal value 
# for p and q, based on ACF and PACF test. Because we already know that
# we have high autocorrelation in our data. A spike in the ACF and PACF in Figure XYZ indicates that we have significant
# spikes for lag 1 in both charts (Hyndman & Athanasopoulos, 2018), meaning that we should set p = q = 1. 

log(ts.train[,2]) %>% 
  diff(lag = 4) %>% 
  diff() %>% 
  ggtsdisplay() # Figure 3

# Based on the resulting assumptions, we conlude that an ARIMA(0,1,3)(0,1,2)[4] might be suitable, because of the significant
# spikes seen in the PACF for lag 3 and 6, determining q = 3 and Q = 3. 

(fit.1 <- Arima(ts.train[,2], order = c(0,1,3), seasonal = c(0,1,2), lambda = BoxCox.lambda(ts.train[,2])))
ce.acf.1 <- ggAcf(fit.1$residuals) + ylab("") + ggtitle("ACF for ARIMA(0,1,3)(0,1,2)")
ce.pacf.1 <- ggPacf(fit.1$residuals) + ylab("") + ggtitle("PACF for ARIMA(1,1,1)(0,1,2)")
ggarrange(ce.acf.1, ce.pacf.1, ncol = 2) # Appendix 5

# The subsequent check shows some autocorrelation left, visible in the ACF plot at spike 6. 
# Because we already adjusted the MA(q) part, this must be a detail to be adjusted in the AR(p) part. 
# We therefore set p = 1 and 2 and compare their AICc, autocorrelation, stationaroty and white noise residuals.

(fit.2 <- Arima(ts.train[,2], order = c(1,1,3), seasonal = c(0,1,2), lambda = BoxCox.lambda(ts.train[,2])))
ce.acf.2 <- ggAcf(fit.2$residuals) + ylab("") + ggtitle("ACF for ARIMA(1,1,3)(0,1,2)")
ce.pacf.2 <- ggPacf(fit.2$residuals) + ylab("") + ggtitle("PACF for ARIMA(1,1,3)(0,1,2)")
ggarrange(ce.acf.2, ce.pacf.2, ncol = 2) # Appendix 6

(fit.3 <- Arima(ts.train[,2], order = c(2,1,3), seasonal = c(0,1,2), lambda = BoxCox.lambda(ts.train[,2])))
ce.acf.3 <- ggAcf(fit.3$residuals) + ylab("") + ggtitle("ACF for ARIMA(2,1,3)(0,1,2)")
ce.pacf.3 <- ggPacf(fit.3$residuals) + ylab("") + ggtitle("PACF for ARIMA(2,1,3)(0,1,2)")
ggarrange(ce.acf.3, ce.pacf.3, ncol = 2) # Figure 4

checkresiduals(fit.1)$p.value # Appendix 7
checkresiduals(fit.2) # Appendix 8
checkresiduals(fit.3) # Figure 5
autoplot(fit.3) # Figure 6

# Comparing our models for ARIMA(1,1,3)(0,1,2)[4] and ARIMA(2,1,3)(0,1,2)[4], we see that the latter has better 
# ACF and PACF spike conditions. Also, the Ljung-Box test is supporting this assumptions as the autocorrelation 
# in the ARIMA(1,1,3)(0,1,2)[4] and ARIMA(0,1,3)(0,1,2)[4] is still highly significant as opposed by the latter model.
# The resulting series is now a white-noise series. The distribution of the residuals also fits the assumed distribution pattern.
# Looking at the coefficients, we observe that ar1, ma1, and sma2 are not statistically significant, and ma3 being almost
# not statistically significant. Because ar1 should not be altered because of high significance of ar2, 
# we try new model variants:

(fit.4 <- Arima(ts.train[,2], order = c(2,1,2), seasonal = c(0,1,1), lambda = BoxCox.lambda(ts.train[,2])))
ce.acf.4 <- ggAcf(fit.4$residuals) + ylab("") + ggtitle("ACF for ARIMA(2,1,2)(0,1,1)") + theme_minimal()
ce.pacf.4 <- ggPacf(fit.4$residuals) + ylab("") + ggtitle("PACF for ARIMA(2,1,2)(0,1,1)") + theme_minimal()
ggarrange(ce.acf.4, ce.pacf.4, ncol = 2) # Appendix 9

# Because in our `fit4` model we still have some issues with autocorrelation we set P = 1 because of the trend 
# that can still be observed in the residuals. We end up with an ARIMA model that also surpasses all previous models
# in terms of coefficient significance, and AICc, AIC, and BIC. Also the Ljung-Box p-value is maximized, as seen in _Figures 7 to 9_:

(fit.5 <- Arima(ts.train[,2], order = c(2,1,2), seasonal = c(1,1,1), lambda = BoxCox.lambda(ts.train[,2])))
ce.acf.5 <- ggAcf(fit.5$residuals) + ylab("") + ggtitle("ACF for ARIMA(2,1,2)(1,1,1)") + theme_minimal()
ce.pacf.5 <- ggPacf(fit.5$residuals) + ylab("") + ggtitle("PACF for ARIMA(2,1,2)(1,1,1)") + theme_minimal()
ggarrange(ce.acf.5, ce.pacf.5, ncol = 2) # Figure 7
checkresiduals(fit.5) # Figure 8
autoplot(fit.5) # Figure 9

(arima.comp.1 <- data.frame(model=c("ARIMA(1,1,1)(0,1,2)", "ARIMA(1,1,3)(0,1,2)", "ARIMA(2,1,3)(0,1,2)", "ARIMA(2,1,2)(0,1,1)", "ARIMA(2,1,2)(1,1,1)"),
                            LB.p.value = c(checkresiduals(fit.1)$p.value, checkresiduals(fit.2)$p.value, checkresiduals(fit.3)$p.value, checkresiduals(fit.4)$p.value, checkresiduals(fit.5)$p.value),
                            aicc = c(fit.1$aicc, fit.2$aicc, fit.3$aicc, fit.4$aicc, fit.5$aicc),
                            bic = c(fit.1$bic, fit.2$bic, fit.3$bic, fit.4$bic, fit.5$bic)) %>% 
    mutate_if(is.numeric, round, digit = 3) %>% 
    arrange(desc(LB.p.value), aicc)) # ordering by p.value and AICc
    
  
# For detailed figures and graphs showing the other scenarios, please see the Appendix. 

# Because KPSS can only be used to determine d and D, we need to employ Information Criteria, such as AICc, to 
# pick the correct p,q,P,Q values. This is already incorporated in the automated ARIMA model selection that 
# calculates different ARIMA models and picks the best models based on those Informaiton Criteria. In fact, the same 
# ARIMA(2,1,2)(1,1,1)[4] is picked, based on the unit root space optimization to guarantee stationarity.
# Also, the obtained ARIMA coefficients remain highly statistically singificant, aside of ma1. 

(fit.arima <- auto.arima(ts.train[,2], lambda = BoxCox.lambda(ts.train[,2])))
summary(fit.arima) # no print
checkresiduals(fit.arima) # no print

# 6. Comparison on ACF and PACF for CE ----

# Comparing the ACF and PACF plots of the raw Final Consumption Expenditure and ARIMA data, we can observe the following:
# 1) The autocorrelation in the residuals is resolved. This was important to resolve, as ARIMA assumes that historical 
# patterns will not change during the forecast. 2) The issue of a high PACF spike at lag 1, indicating 
# correlation between the error terms of consumption between different lags was resolved. This is important to resolve, because
# ARIMA assumes uncorrelated future errors.

ce.acf <- ggAcf(ts.train[,2]) + ylab("") + ggtitle("ACF for Final Consumption Exp. (in million AUD)")
ce.pacf <- ggPacf(ts.train[,2]) + ylab("") + ggtitle("PACF for Final Consumption Exp. (in million AUD)")
ggarrange(ce.acf, ce.pacf,
          ce.acf.5, ce.pacf.5, 
          ncol = 2, nrow = 2) # Figure 10

# 7. Data Forecast ----

autoplot(forecast(fit.5, h =39), series = "Forecast") +
  autolayer(test[,2], series = "Actual") +
  ggtitle("Final Consumption Expenditure Prediction",
          subtitle = "Forecast fits actual data quite well, despite slight overestimation. PI increase due to included differences") +
  xlab("Year") +
  ylab("Consumption Exp. (in million AUD)") +
  scale_x_continuous(limits = c(1990,2020))

# 8. Forecast with income as explanatory, and new ARIMA model ----

# The inclusion of a new explanatory variable in the ARIMA model requires us to check the errors terms of the regression model
# (eta) and our ARIMA model (epsilon). In our case, our two variables for consumption and income are cointegrated. That's
# why we can rely on non-stationary time series (Hyndman & Athanasopoulos, 2018). In our first model, that is already
# adjusted with d = D = 1, as we observed with the KPSS test before in order to guarantee non-stationarity of the data,
# we still observe significant ACF spikes for lag 1,3, and 4, suggesting a Q-value of 1. PACF spikes for lag 1,3, and 4
# also indicate that P = 1.

(fit.arima.adv.1 <- Arima(ts.train[,2], 
                          order = c(0,1,0), 
                          seasonal = c(0,1,0), 
                          xreg = ts.train[,3], 
                          lambda = BoxCox.lambda(ts.train[,2])))

ce.acf.adv.1 <- ggAcf(fit.arima.adv.1$residuals) + 
  ylab("") + 
  ggtitle("ACF for Regression w/ ARIMA(0,1,0)(0,1,0)")
ce.pacf.adv.1 <- ggPacf(fit.arima.adv.1$residuals) + 
  ylab("") + 
  ggtitle("PACF for Regression w/ ARIMA(0,1,0)(0,1,0)")

ggarrange(ce.acf.adv.1,ce.pacf.adv.1, ncol = 2) # Figure XYZ
checkresiduals(fit.arima.adv.1) # Appendix XYZ

# Comparison of the ARIMA models ----
(fit.arima.adv.2 <- Arima(ts.train[,2], 
                          order = c(0,1,0), 
                          seasonal = c(1,1,1), 
                          xreg = ts.train[,3], 
                          lambda = BoxCox.lambda(ts.train[,2])))


ce.acf.adv.2 <- ggAcf(fit.arima.adv.2$residuals) + 
  ylab("") + 
  ggtitle("ACF for Regression w/ ARIMA(0,1,0)(1,1,1)")
ce.pacf.adv.2 <- ggPacf(fit.arima.adv.2$residuals) + 
  ylab("") + 
  ggtitle("PACF for Regression w/ ARIMA(0,1,0)(1,1,1)")

ggarrange(ce.acf.adv.2,ce.pacf.adv.2, ncol = 2) # Appendix XYZ 
checkresiduals(fit.arima.adv.2) # Appendix XYZ

# This try shows us significant ACF and PACF spikes for lags 2 and 3 as well as 6. We set p = q = 2, as in the previous model
# to balance the ACF and PACF values against each other. This results in optimal models considering ACF/PACF and white-noise
# behaviour, residual distribution, heteroscedasticity, stationarity, and coefficient significance:

# When looking at the coefficients, we observe that ma1 and xreg, are not significant, but we include the latter it
# because of the task. We do not delete ma1, as this would impact the significant ma2 coefficient and because of
# the needed transformation towards autocorrelation decrese.

(fit.arima.adv.3 <- Arima(ts.train[,2], 
                          order = c(2,1,2), 
                          seasonal = c(1,1,1), 
                          xreg = ts.train[,3], 
                          lambda = BoxCox.lambda(ts.train[,2])))

ce.acf.adv.3 <- ggAcf(fit.arima.adv.2$residuals) + 
  ylab("") + 
  ggtitle("ACF for Regression w/ ARIMA(2,1,2)(1,1,1)")

ce.pacf.adv.3 <- ggPacf(fit.arima.adv.2$residuals) + 
  ylab("") + 
  ggtitle("PACF for Regression w/ ARIMA(2,1,2)(1,1,1)")

ggarrange(ce.acf.adv.3,ce.pacf.adv.3, ncol = 2) # Figure XYZ

checkresiduals(fit.arima.adv.3) # Figure XYZ

cbind("Regression Errors (eta_t)" = residuals(fit.arima.adv.3, type = "regression"),
      "ARIMA Errors (epsilon_t)" = residuals(fit.arima.adv.3, type = "innovation")) %>% 
  autoplot(facets = T) +
  ggtitle("Comparison of Forecast and ARIMA(2,1,2)(1,1,1) Errors",
          subtitle = "Regression Errors capute the overall time series trend, ARIMA errors resemlbe a white noise series") # Fig XYZ

# This reestimation yields in a suitable model, considering the white noise type of ARIMA residuals, ACF and PACF specifics,
# as well as a fitting residual distribution that is only slightly skewed because of the observation outliers during
# the financial crisis in 2007/08. Additionally, we could include a constant in order
# to mimick the trend that is displayed in our regression residuals. For this, and because a drift cannot be included
# if the order of difference > 2, we must set d = 0 and also q = 0, because this drift should explain the information
# conveyed in the regression residuals. But because this change yields in more autocorrelation, we refrain from doing so:

# On the other side, the automated approach yields in a different model variation, that was already discussed above
# but discarded because of its negative impact on ACF and PACF plots and white-noise properties. It yields a
# lower AICc and does not yield in autocorrelation reduction, even though the ARIMA errors are white noise based (_Figure XYZ_).
# In sum, the automated is a worse forecast model than our ARIMA(2,1,2)(1,1,1) model:

(fit.arima.adv <- auto.arima(ts.train[,2], xreg = ts.train[,3], lambda = BoxCox.lambda(ts.train[,2])))

cbind("Regression Errors (eta_t)" = residuals(fit.arima.adv, type = "regression"),
      "ARIMA Errors (epsilon_t)" = residuals(fit.arima.adv, type = "innovation")) %>% 
  autoplot(facets = T) +
  ggtitle("Comparison of Forecast and ARIMA Errors",
          subtitle = "ARIMA errors resemble white noise series, while Regression errors contian trends and cycles") # Figure XYZ

checkresiduals(fit.arima.adv) # Appendix XYZ

(arima.comp.2 <- data.frame(model=c("ARIMA(0,1,0)(0,1,0)", "ARIMA(0,1,0)(1,1,1)", "ARIMA(2,1,2)(1,1,1)", "ARIMA(1,1,1)(0,0,2)"),
                            LB.p.value = c(checkresiduals(fit.arima.adv.1)$p.value, checkresiduals(fit.arima.adv.2)$p.value, checkresiduals(fit.arima.adv.3)$p.value, checkresiduals(fit.arima.adv)$p.value),
                            aicc = c(fit.arima.adv.1$aicc, fit.arima.adv.2$aicc, fit.arima.adv.3$aicc, fit.arima.adv$aicc),
                            bic = c(fit.arima.adv.1$bic, fit.arima.adv.2$bic, fit.arima.adv.3$bic, fit.arima.adv$bic)) %>% 
    mutate_if(is.numeric, round, digit = 3) %>% 
    arrange(desc(LB.p.value), aicc)) # ordering by p.value and AICc

# Forecasting Comparison
fcs.adv.3 <- forecast(fit.arima.adv.3, xreg = ts.test[,3])
fcs.adv <- forecast(fit.arima.adv, xreg = ts.test[,3])

fcs.adv.3.plot <- autoplot(fcs.adv.3, series = "Fitted Consumption") +
  autolayer(ts.test[,2], series = "Actual Consumption") +
  xlab("Years") +
  ylab("Consumption (in million AUD)") +
  scale_x_continuous(limits = c(1990,2020))

fcs.adv.plot <- autoplot(fcs.adv, series = "Fitted Consumption") +
  autolayer(ts.test[,2], series = "Actual Consumption") +
  xlab("Years") +
  ylab("Consumption (in million AUD)") +
  scale_x_continuous(limits = c(1990,2020))

ggarrange(fcs.adv.3.plot, fcs.adv.plot, nrow = 2)


# 9. Consumption foreacsting with average forecast and optimal weights ----
# Dynamic and ARIMA ----
# Averaging: dynamic regression and ARIMA
fcs.5 <- forecast(fit.5, h = 40)
comb_dr <- (fcs.5[["mean"]]+fcs.adv.3[["mean"]])/2

c_1 <- autoplot(tse[,2]) +
  autolayer(comb_dr, series = "Combination (AVG)")+
  ggtitle("Consumption Prediction Comparison", 
          subtitle = "Based on averaging of Dynamic Regression and Manual ARIMA") +
  xlab("Year") +
  ylab("Consumption Exp. (in million AUD)") +
  scale_x_continuous(limits = c(2000,2020))

accuracy(fcs.5, x = ts.test[,2])
accuracy(fcs.adv.3, x = ts.test[,2])
accuracy(comb_dr, x = ts.test[,2])

# Optimal weights: dynamic regression and ARIMA
w <- (var(fcs.adv.3[["mean"]]) - sd(fcs.adv.3[["mean"]]+fcs.5[["mean"]]))/(var(fcs.5[["mean"]])+var(fcs.adv.3[["mean"]])-2*sd(fcs.adv.3[["mean"]]+fcs.5[["mean"]]))
rw <- 1-w

comb_dr_w <- w*fcs.5[["mean"]]+rw*fcs.adv.3[["mean"]]
accuracy(comb_dr_w, x = ts.test[,2])

# ARIMA and autoARIMA comparison ----
# Averaging: Auto and Manual
comb_am <- (fcs.adv[["mean"]]+fcs.adv.3[["mean"]])/2

c_2 <- autoplot(tse[,2]) +
  autolayer(comb_am, series = "Combination (AVG)")+
  ggtitle("Consumption Prediction Comparison", 
          subtitle = "Based on averaging of AutoARIMA and Manual ARIMA") +
  xlab("Year") +
  ylab("Consumption Exp. (in million AUD)") +
  scale_x_continuous(limits = c(2000,2020))


accuracy(fcs.adv, x = ts.test[,2])
accuracy(fcs.adv.3, x = ts.test[,2])
accuracy(comb_am, x = ts.test[,2])

# Optimal weights: Auto and Manual
w <- (var(fcs.adv.3[["mean"]]) - sd(fcs.adv.3[["mean"]]+fcs.adv[["mean"]]))/(var(fcs.adv[["mean"]])+var(fcs.adv.3[["mean"]])-2*sd(fcs.adv.3[["mean"]]+fcs.adv[["mean"]]))
rw <- 1-w

comb_am_w <- w*fcs.adv[["mean"]]+rw*fcs.adv.3[["mean"]]
accuracy(comb_am_w, x = ts.test[,2])

# With ETS and TBATS ----
# Averaging: ETS, TBATS
ETS <- forecast(ets(ts.train[,2]), h = 40)
TBATS <- forecast(tbats(ts.train[,2], biasadj = T), h = 40)
comb_tbats <- (fcs.adv.3[["mean"]]+TBATS[["mean"]])/2
comb_ets <- (fcs.adv.3[["mean"]]+ETS[["mean"]])/2

c_3 <- autoplot(tse[,2]) +
  autolayer(fcs.adv.3, series = "Manual Dynamic Regression", PI = F) +
  autolayer(comb_tbats, series = "M. Dynamic Reg. + TBATS (AVG)")+
  autolayer(comb_ets, series = "M. Dynamic Reg. + ETS (AVG)")+
  ggtitle("Consumption Prediction Comparison",
          subtitle = "based on Manual ARIMA and ARIMA Combinations") +
  xlab("Year") +
  ylab("Consumption Exp. (in million AUD)") +
  scale_x_continuous(limits = c(2000,2020))

accuracy(comb_ets, x = ts.test[,2])
accuracy(comb_tbats, x = ts.test[,2])

# Optimal weights: ETS, TBATS
# For TBATS
w <- (var(fcs.adv.3[["mean"]]) - sd(fcs.adv.3[["mean"]]+TBATS[["mean"]]))/(var(TBATS[["mean"]])+var(fcs.adv.3[["mean"]])-2*sd(fcs.adv.3[["mean"]]+TBATS[["mean"]]))
rw <- 1-w

comb_tbats_w <- w*TBATS[["mean"]]+rw*fcs.adv.3[["mean"]]
accuracy(comb_tbats_w, x = ts.test[,2])

# For ETS
w <- (var(fcs.adv.3[["mean"]]) - sd(fcs.adv.3[["mean"]]+ETS[["mean"]]))/(var(ETS[["mean"]])+var(fcs.adv.3[["mean"]])-2*sd(fcs.adv.3[["mean"]]+ETS[["mean"]]))
rw <- 1-w

comb_ets_w <- w*ETS[["mean"]]+rw*fcs.adv.3[["mean"]]
accuracy(comb_ets_w, x = ts.test[,2])

# 10. Make plot with data and forecasts ----
# ARIMA + Dynamic ARIMA ----
# Comparison with sole models
c_1 +
  autolayer(fcs.5, series = "ARIMA(2,1,2)(1,1,1)[4]", PI = F) +
  autolayer(fcs.adv.3, series = "Manual Dynamic Regression", PI = F)

# Average vs. optimal values
c_1 +
  autolayer(comb_dr_w, series = "Combination (OW)")
  
# ARIMA Manual + ARIMA Auto ----
# Comparison with sole models
c_2 +
  autolayer(fcs.adv, series = "Auto Dynamic Regression", PI = F)+
  autolayer(fcs.adv.3, series = "Manual Dynamic Regression", PI = F)

# Average vs. optimal values
c_2 +
  autolayer(comb_am_w, series = "Combination (OW)")

# ARIMA+ETS and ARIMA+TBATS ----
# Comparison with sole models
c_3 +
  autolayer(ETS, series = "ETS", PI = F)+
  autolayer(TBATS, series = "TBATS", PI = F)

# Average vs. optimal values
c_3 +
  autolayer(comb_ets_w, series = "ETS + M. Dynamic Regression (WO)") +
  autolayer(comb_tbats_w, series = "TBATS + M. Dynamic Regression (WO)")

autoplot(tse[,2]) +
  autolayer(comb_ets, series = "M. Dynamic regression + ETS (AVG)")+
  autolayer(comb_ets_w, series = "M. Dynamic regression + ETS (WO)")+
  autolayer(fcs.adv.3, series = "Manual Dynamic Regression", PI = F)+
  ggtitle("Consumption Prediction Comparison") +
  xlab("Year") +
  ylab("Consumption Exp. (in million AUD)") +
  scale_x_continuous(limits = c(2000,2020))

# 11. Forecast measures and comparison ----
all_comp <- as.data.frame(matrix(data = c(accuracy(fcs.5, x = ts.test[,2])["Test set", c("ME", "RMSE", "MAE")],
                            accuracy(fcs.adv, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(fcs.adv.3, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(ETS, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(TBATS, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(comb_dr, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(comb_dr_w, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(comb_am, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(comb_am_w, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(comb_ets, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(comb_ets_w, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(comb_tbats, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")],
                            accuracy(comb_tbats_w, x = ts.test[,2])["Test set",c("ME", "RMSE", "MAE")]),
                   nrow = 13,
                   ncol = 3,
                   byrow = T,
                   dimnames = list(c("ARIMA", 
                                     "DR (auto)",
                                     "DR (manual)",
                                     "ETS", 
                                     "TBATS",
                                     "ARIMA + DR (AVG)",
                                     "ARIMA + DR (WO)",
                                     "DR (Auto + Manual) (AVG)",
                                     "DR (Auto + Manual) (WO)",
                                     "DR + ETS (AVG)",
                                     "DR + ETS (WO)",
                                     "DR + TBATS (AVG)",
                                     "DR + TBATS (WO)"), 
                                   c("ME", "RMSE", "MAE"))))

all_comp <- all_comp[order(all_comp$RMSE),]
all_comp$Model <- factor(row.names(all_comp), levels = row.names(all_comp)) 

ggplot(all_comp, aes(x=Model, y= RMSE, label=RMSE)) +
  scale_x_discrete(limits = rev(levels(all_comp$Model)))+
  geom_bar(stat='identity', aes(fill=row.names(all_comp)), width=.5)+
  theme(legend.position = "none")+
  labs(subtitle="ETS and Weighted ETS + M. Dynamic Reg. perform the best followed by sole MDR", 
       title= "Comparison of forecasts accuracy, based on RMSE for combined and sole models") + 
  coord_flip()
