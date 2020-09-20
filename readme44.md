Goals and Procedure
-------------------

The aim of this short project was to investigate the behavior of
statistical time-series models and their mathematical assumptions. Main
subject of this study are ARIMA and SARIMA models that were used to
understand the relationship between consumption and GDP in Australia.
Additionally, forecast combinations were derived to identify further
modelling variants.

Furthermore, the performance of the models was compared to ETS, TBATS,
Average. DR+ARIMA etc. The results indicated, that DR+ETS approaches
performed best considering the given data.

Applied Methodologies (excerpt)
-------------------------------

1.  Analysis of Trend, Seasons, and Spurious Regression Issues (KPSS,
    ADF)
2.  Stationarity Analysis
3.  Autocorrelation and Residual Analysis
4.  Box-Cox Transformation

![](readme_files/figure-markdown_github/Figure%201%20Disposable%20income%20Analysis-1.png)

![](readme_files/figure-markdown_github/Figure%202%20residuals%20analysis-1.png)

![](readme_files/figure-markdown_github/Figure%203%20log-diff-1.png)

![](readme_files/figure-markdown_github/Figure%204%20ARIMA%20(2,1,3)(0,1,2)-1.png)

![](readme_files/figure-markdown_github/Figure%205%20residual%20analysis-1.png)

Because in our `fit4` model (see *Appendix*, *Figure 28*) we still have
some issues with autocorrelation we set `P` = 1 because of the trend
that can still be observed in the residuals. We design and end up with
an ARIMA(2,1,2)(1,1,1) model that also surpasses all previous models in
terms of coefficient significance, and AICc, AIC, and BIC. Also the
Ljung-Box p-value is maximized, the residual distibution feed the ARIMA
process requirements, and the unit root theorem is also satisfied, as
seen in *Figures 6 to 8*:

![](readme_files/figure-markdown_github/Figure%206%20model,%20ACF%20and%20PACF-1.png)

![](readme_files/figure-markdown_github/Figure%207%20residual%20analysis-1.png)

![](readme_files/figure-markdown_github/Figure%208%20characterisitc%20roots-1.png)

![ACF and PACF Comparison between Original Data and ARIMA Transformed
Results](readme_files/figure-markdown_github/Figure%202%20ACF%20and%20PACF%20comparison-1.png)

![](readme_files/figure-markdown_github/ARIMA%20forecast-1.png)

![](readme_files/figure-markdown_github/Comparison%20of%20ARIMA%20DR-1.png)

![](readme_files/figure-markdown_github/Figure%2012%20ACF%20and%20PACF%20comparison-1.png)

![](readme_files/figure-markdown_github/Figure%2013%20residual%20analysis%20DR-1.png)

![](readme_files/figure-markdown_github/Figure%2014%20errors%20comparison-1.png)

![](readme_files/figure-markdown_github/Figure%2015%20errors%20comparison-1.png)

![](readme_files/figure-markdown_github/Figure%2017%20forecasts%20comparison-1.png)

As for the first combination case, where the ARIMA model is combined
with the same model with regression, we can observe that the RMSE for
the sole models is comparably close. So, in fact, there is no direct
need to combine the forecasts, as the performance of the combined models
and sole models will differ just slightly.

![Forecast Combination for Dynamic Regressions Using
ARIMA](readme_files/figure-markdown_github/ARIMA%20and%20DR%20plot-1.png)

![Forecast Combination for Dynamic Regressions and Automated
Regression](readme_files/figure-markdown_github/Manual%20and%20autoARIMA%20plot-1.png)

![Forecast Comparison for ETS, TBATS
Approaches](readme_files/figure-markdown_github/unnamed-chunk-1-1.png)

![Forecast Accuracy
Comparison](readme_files/figure-markdown_github/Figure%20Measurements%20comparison-1.png)
