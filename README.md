
# meteobr <a href="https://github.com/carlosdemoura/meteobr"><img src="logo_meteobr/meteobr.png" align="right" height="138" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/carlosdemoura/meteobr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/carlosdemoura/meteobr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `meteobr` is to provide an easy way to get data from the automated meteorological stations of Brazil's [National Institute of Meteorology](https://portal.inmet.gov.br/). These stations provide, at each hour of every day between 2000-2024, a plethora of useful data (see the vignette [data trivia](https://carlosdemoura.github.io/meteobr/articles/data_trivia.html)).

## Installation

You can install the development version of meteobr from [GitHub](https://github.com/carlosdemoura/meteobr/) with:

``` r
# install.packages("devtools")
devtools::install_github("carlosdemoura/meteobr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(meteobr)

get_data(first.day = "2001-01-01", last.day = "2001-01-02")
```
