# Download preprocessed data

`set_data_locally()` downloads .Rdata files and stores it locally
without the hassle of unzipping and processing them.

## Usage

``` r
set_data_locally(years = 2000:meteobr.max.year)
```

## Arguments

- years:

  Vector of integers between 2000 and 2024; if no value is passed, the
  DEFAULT is to download all data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Should download all data available on
# <https://github.com/carlosdemoura/meteobr/raw/refs/heads/master/data/repo/>
# and store it where `local_data()` says.
set_data_locally()
} # }
```
