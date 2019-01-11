
tstaxonomyr
===========

A time series taxonomy to classify univariate or multivariate time series based on 24 different (statistical) time series features. The taxonomy function of this package 'classify\_ts' generates and collects all feature values. Next the feature values are scaled to \[0,1\] and then assigned to the defined taxonomy factors. Additionally, each feature can be calculated for it own.

Usage
-----

``` r
# Initialize the R package
library(tstaxonomyr)

# Example of the TS taxonomy classification --------
# Use the univariate time series object 'BJsales'
ts_sales = datasets::BJsales
# Classify the time series based on the defined taxonomy 
# in 'classify_ts'
classified_ts <- classify_ts(ts = ts_sales, na_option = "mean")
# Get the classification factor results
classified_ts

# Example of a single feature value calculation --------
# Calculate the skewness of a time series object
skewness = calculate_skewness(ts = ts_sales)
# Get the resulting skewness factor
skewness
```

Installation
------------

You can install the **development** version 1.0.0 from [Github](https://github.com/mowomoyela/tstaxonomyr) with:

``` r
devtools::install_github("mowomoyela/tstaxonomyr")
```

Overview
--------

All provided functions of this package:

-   Version 1.0.0
    -   classify\_ts: Classifies a time series based on the defined ts taxonomy features.
    -   calculate\_skewness: Generates the skewness of an ts object.
    -   calculate\_kurtosis: Generates the kurtosis of an ts object.
    -   calculate\_trend: Generates the trend of an ts object.
    -   calculate\_autocorrelation: Generates the mean of the autocorrelationfunction (ACF) of an ts object.
    -   calculate\_mean: Generates the mean of the normalized values \[0,1\] of an ts object.
    -   calculate\_sd: Generates the standard deviation of an ts object.
    -   calculate\_observationnumber: Generates the number of observations of an ts object.
    -   calculate\_non\_linearity: Generates the non linearity factor of an ts object.
    -   calculate\_seasonality: Generates the seasonality factor of an ts object.
    -   calculate\_periodicity: Generates the periodicity/frequency factor of an ts object.
    -   calculate\_chaos: Generates the maximum Lyapunov exponent (chaos) of an ts object.
    -   calculate\_entropy: Generates the approximate entropy of an ts object.
    -   calculate\_selfsimilarity: Generates the Hurst exponent of an ts object.
    -   calculate\_dtw\_blockdistance: Generates the dynamic time warping (DTW) for an ts object to the 1000 ts from the list in /data.
    -   calculate\_turningpoint\_percentage: Generates the percentage of turning points of an ts object.
    -   calculate\_partial\_autocorrelation: Generates the mean of the partial autocorrelationfunction (PACF) of an ts object.
    -   calculate\_variance: Generates the variance of an ts object.
    -   calculate\_outlier\_percentage: Generates the percentage of outliers of an ts object.
    -   calculate\_stepchange\_percentage: Generates the percentage of step changes of an ts object.
    -   calculate\_peak\_percentage: Generates the percentage of peaks of an ts object.
    -   calculate\_durbin\_watson\_test: Generates the autocorrelation measure of an data.frame object.
    -   calculate\_quartile\_distribution: Generates the percentage of the values in the 4 quartiles of an ts object.
    -   calculate\_determination\_coefficient: Generates the coefficient of determination (R2) of an data.frame object.
    -   calculate\_attributenumber: Generates the number of attributes of an data.frame object.
    -   get\_ts\_frequency: Get the period of an vector object.
    -   decompose\_ts: Decompose a time series object into trend, seasonal and remainder.
    -   scale\_feature: Scale a numeric value into a standardized interval \[0,1\].
    -   get\_ts\_list: Get the time series list containing 1000 time series data.
    -   get\_blocklist: Get the blocklist with the matrices for the DTW distance calucation.

License
-------

This package is free and open source software, licensed under GPL-2.
