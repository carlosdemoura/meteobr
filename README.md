
<!-- README.md is generated from README.Rmd. Please edit that file -->

# meteobr

<!-- badges: start -->
<!-- badges: end -->

The goal of meteobr is to …

## Installation

You can install the development version of meteobr like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

```math
\begin{tabular}{ll}
precipitation        & PRECIPITAÇÃO TOTAL, HORÁRIO (mm) \\
atm_pressure         & PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB) \\
atm_pressure_max     & PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB) \\
atm_pressure_min     & PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB) \\
radiation            & RADIACAO GLOBAL (Kj/m²) \\
temperature_air      & TEMPERATURA DO AR - BULBO SECO, HORARIA (°C) \\
temperature_dew      & TEMPERATURA DO PONTO DE ORVALHO (°C) \\
temperature_max      & TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C) \\
temperature_min      & TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C) \\
temperature_dew_max  & TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C) \\
temperature_dew_min  & TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C) \\
humidity_max         & UMIDADE REL. MAX. NA HORA ANT. (AUT) (%) \\
humidity_min         & UMIDADE REL. MIN. NA HORA ANT. (AUT) (%) \\
humidity             & UMIDADE RELATIVA DO AR, HORARIA (%) \\
wind_direction       & VENTO, DIREÇÃO HORARIA (gr) (° (gr)) \\
wind_burst_max       & VENTO, RAJADA MAXIMA (m/s) \\
wind_burst           & VENTO, VELOCIDADE HORARIA (m/s) \\
\end{tabular}
```


## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(meteobr)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
