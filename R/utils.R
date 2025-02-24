
#shas local files

fiat_time_pattern = function(every.time) {
  build_regex = function(values, digits) {
    if (is.null(values)) return(stringr::str_c("\\d{", digits, "}"))
    values = values |>
      as.character() |>
      stringr::str_pad(digits, pad = "0")
    return(stringr::str_c("(", stringr::str_c(values, collapse = "|"), ")"))
  }

  stringr::str_c(
    build_regex(every.time$year,  4), "/",
    build_regex(every.time$month, 2), "/",
    build_regex(every.time$mday,  2), " ",
    build_regex(every.time$hour,  2)
  )
}


adjust_lines_csv = function(year, first.day, last.day) {
  between.days = c(first.day, last.day)

  for (i in 1:2) {
    if (is.na(between.days[i])) {
      between.days[i] = year |>
        {\(.) if (i == 1) paste0(., "/01/01")
          else paste0(., "/12/31")}()
    }
  }

  csv.lines = lubridate::yday(between.days)

  first_day_2000 = lubridate::yday("2000-05-07")

  if (year == 2000) {
    if (csv.lines[2] < first_day_2000) {
      return( data.frame() )
    }

    if (csv.lines[1] < first_day_2000) {
      csv.lines[1] = 1
    } else {
      csv.lines[1] = csv.lines[1] - first_day_2000 + 1
    }

    csv.lines[2] = csv.lines[2] - first_day_2000 + 1
  }

  csv.lines[1] = csv.lines[1] - 1
  csv.lines = (csv.lines * 24) + 9

  csv.lines
}


drop_stations = function(x) {
  x
}


validate_dates = function(year = NULL, first.day, last.day) {
  pattern_w_year = "\\d{4}[-/]\\d{2}[-/]\\d{2}"
  pattern_wo_year = "\\d{2}[-/]\\d{2}"

  stopifnot( "days must both include/exclude year" =
               all(
                 grepl(pattern_w_year,  first.day) == grepl(pattern_w_year,  last.day),
                 grepl(pattern_wo_year, first.day) == grepl(pattern_wo_year, last.day)
                 )
             )

  stopifnot( "days must in the format yyyy/mm/dd or mm/dd" =
               any(
                 grepl(pattern_w_year,  first.day),
                 grepl(pattern_wo_year, first.day)
                 )
             )

  if (grepl(pattern_w_year, first.day)) {
    stopifnot( "all days must be in the same year (and equal to the specified year)" =
                 all(
                   lubridate::year(first.day) == lubridate::year(last.day),
                   {if (!is.null(year)) lubridate::year(first.day) == year else T}
                 )
               )
  } else if (grepl(pattern_wo_year, first.day)) {
    first.day = paste0(year, "/", first.day)
    last.day  = paste0(year, "/", last.day)
  }

  stopifnot( "last.day must be after first.day" =
             lubridate::yday(last.day) > lubridate::yday(first.day) )

  list(first.day, last.day)
}


fiat_years = function(first.day, last.day) {
  years = lubridate::year(c(first.day, last.day)) %>%
    {seq(.[1], .[2])}

  y = list()
  if (length(years) == 1) {
    y[[ as.character(years) ]] = list(
      first.day = first.day,
      last.day  = last.day
    )
  } else {
    for (i in 1:length(years)) {
      if (i == 1) {
        y[[ as.character(years[i]) ]] = list(
          first.day = first.day,
          last.day  = paste0(years[1], "/31/12")
        )
      } else if (i == length(years)) {
        y[[ as.character(years[i]) ]] = list(
          first.day = paste0(years[i], "/01/01"),
          last.day  = last.day
        )
      } else {
        y[[ as.character(years[i]) ]] = list(
          first.day = paste0(years[i], "/01/01"),
          last.day  = paste0(years[i], "/31/12")
        )
      }
    }
  }

  y
}


inmet.args = list(
  "date"                 = "Data",
  "hour"                 = "Hora UTC",
  "precipitation"        = "PRECIPITAÇÃO TOTAL, HORÁRIO (mm)",
  "atm_pressure"         = "PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)",
  "atm_pressure_max"     = "PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)",
  "atm_pressure_min"     = "PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)",
  "radiation"            = "RADIACAO GLOBAL (Kj/m²)",
  "temperature_air"      = "TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)",
  "temperature_dew"      = "TEMPERATURA DO PONTO DE ORVALHO (°C)",
  "temperature_max"      = "TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)",
  "temperature_min"      = "TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C)",
  "temperature_dew_max"  = "TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C)",
  "temperature_dew_min"  = "TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C)",
  "humidity_max"         = "UMIDADE REL. MAX. NA HORA ANT. (AUT) (%)",
  "humidity_min"         = "UMIDADE REL. MIN. NA HORA ANT. (AUT) (%)",
  "humidity"             = "UMIDADE RELATIVA DO AR, HORARIA (%)",
  "wind_direction"       = "VENTO, DIREÇÃO HORARIA (gr) (° (gr))",
  "wind_burst_max"       = "VENTO, RAJADA MAXIMA (m/s)",
  "wind_burst"           = "VENTO, VELOCIDADE HORARIA (m/s)"
)


inmet.args2 = list(
  "Data"                                                  = "date",
  "Hora UTC"                                              = "hour",
  "PRECIPITAÇÃO TOTAL, HORÁRIO (mm)"                      = "precipitation",
  "PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)" = "atm_pressure",
  "PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)"       = "atm_pressure_max",
  "PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)"      = "atm_pressure_min",
  "RADIACAO GLOBAL (Kj/m²)"                               = "radiation",
  "TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)"          = "temperature_air",
  "TEMPERATURA DO PONTO DE ORVALHO (°C)"                  = "temperature_dew",
  "TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)"            = "temperature_max",
  "TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C)"            = "temperature_min",
  "TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C)"      = "temperature_dew_max",
  "TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C)"      = "temperature_dew_min",
  "UMIDADE REL. MAX. NA HORA ANT. (AUT) (%)"              = "humidity_max",
  "UMIDADE REL. MIN. NA HORA ANT. (AUT) (%)"              = "humidity_min",
  "UMIDADE RELATIVA DO AR, HORARIA (%)"                   = "humidity",
  "VENTO, DIREÇÃO HORARIA (gr) (° (gr))"                  = "wind_direction",
  "VENTO, RAJADA MAXIMA (m/s)"                            = "wind_burst_max",
  "VENTO, VELOCIDADE HORARIA (m/s)"                       = "wind_burst"
)


