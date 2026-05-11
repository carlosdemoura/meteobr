# Get meteorological data within interval

`get_data()` does this. Beware that this function, check
[`local_data()`](https://carlosdemoura.github.io/meteobr/reference/local_data.md)
to see more.

## Usage

``` r
get_data(first.day, last.day, vars = NULL, stations = NULL)
```

## Arguments

- first.day, last.day:

  String like "yyyy-mm-dd".

- vars:

  character vector; variables to be collected; if NULL (DEAFULT) all
  variables are collected.

- stations:

  character vector; stations to be collected; if NULL (DEAFULT) data
  from all stations is collected.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
containing the required data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Should return `humidity` data from `station` A001 collected between 2019-april-02
# & 2022-november-08.
get_data(first.day = "2019-04-02", last.day = "2022-11-08", vars = "humidity", stations = "A001")
} # }
```
