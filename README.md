Overview <img src="inst/images/logo.png" align="right" />
--------

[![Build Status](https://travis-ci.org/zappingseb/analyze_last_fm.svg?branch=master)](https://travis-ci.org/zappingseb/analyze_last_fm)

The **analyzelastfm** is a package to help you downloading your data from the last.fm REST API by simple R calls.
It is written in R6.


Installation
------------

``` r

# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("zappingseb/analyze_last_fm")
```

Usage
-----

to use this package you need to have a last.fm API key. You can get one from:
https://www.last.fm/api/account/create

``` r
library(analyzelastfm)
my2017data <- UserData$new("myuser","xxx0000xxxKEY",2017)

# Remove duplicates that can happen due to problems of last.fm and Spotify API
my2017data$clean_data_duplicates()

View(my2017data$albumstats(sort_by="by_total_count"))
```

First Plotting functions
---
**Clock Plot**

``` r
my2017data$clock.plot()

```
![alt text](https://raw.githubusercontent.com/zappingseb/analyze_last_fm/master/inst/images/clock_plot.png)

**Bar Chart**

``` r
my2017data$barplots("weekdays")

```
![alt text](https://raw.githubusercontent.com/zappingseb/analyze_last_fm/master/inst/images/weekdays.png)

``` r
my2017data$daily_month_plot()

```

![alt text](https://raw.githubusercontent.com/zappingseb/analyze_last_fm/master/inst/images/month_day.png)
