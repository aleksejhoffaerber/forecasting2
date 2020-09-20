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

![](readme_files/figure-markdown_github/Figure%208%20characterisitc%20roots-1.png)

![ACF and PACF Comparison between Original Data and ARIMA Transformed
Results](readme_files/figure-markdown_github/Figure%202%20ACF%20and%20PACF%20comparison-1.png)

![Forecast Combination for Dynamic Regressions Using
ARIMA](readme_files/figure-markdown_github/ARIMA%20and%20DR%20plot-1.png)

![Forecast Combination for Dynamic Regressions and Automated
Regression](readme_files/figure-markdown_github/Manual%20and%20autoARIMA%20plot-1.png)

![Forecast Comparison for ETS, TBATS
Approaches](readme_files/figure-markdown_github/unnamed-chunk-1-1.png)

![Forecast Accuracy
Comparison](readme_files/figure-markdown_github/Figure%20Measurements%20comparison-1.png)
