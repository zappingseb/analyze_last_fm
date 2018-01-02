Overview
--------

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
data <- UserData$new("myuser","xxx0000xxxKEY",2017)

View(data$albumstats(sort_by="by_total_count"))
```

