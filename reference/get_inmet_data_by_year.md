# Get data from INMET website

`get_inmet_data_by_year()` downloads data of a specific year from the
official INMET website and prepreprocess it.

## Usage

``` r
get_inmet_data_by_year(
  year,
  first.day = NA,
  last.day = NA,
  vars = NULL,
  stations = NULL
)
```

## Arguments

- year:

  numeric; integer number between 2000 and 2024.

- first.day, last.day:

  character in the format "mm-dd". If NA (default), the first/last day
  of the year is considered.

- vars:

  character vector; variables to be collected. If NULL (default), all
  variables are collected.

- stations:

  character vector; stations to be collected. If NULL (default), all
  stations are collected.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
containing the required data. Errors may arise if:

- `year` is not specified.

- You're trying to collect data before 2000-May-07.

- `fist.day` doesn't comes before `last.day`.

- Just one of `first.day` & `last.day` is passed.

- Your PC is not connected to the internet.

## First day in record

If you're trying to collect data of the year 2000 and the `first.day` is
before 2000-May-07, you're only getting data from 2000-May-07 and beyond
because that was (allegedly) the day INMET automatic stations began
operations. Note that some stations began collecting actual data some
days after 2000-May-07.

## Examples

``` r
if (FALSE) { # \dontrun{
# `first.day` & `last.day` must be in format mm-dd, here the code wouldn't run
# because it is in the format yyyy-mm-dd.
get_inmet_data_by_year(2000, first.day = "2000-01-01", last.day = "2000-12-31")

# This Should get all 2000 data.
get_inmet_data_by_year(2000)
} # }
```
