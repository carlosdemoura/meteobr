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


validate_dates = function(year = NULL, first.day, last.day) {
  dplyr::`%>%`

  pattern_wo_year = "^\\d{2}[-/]\\d{2}$"

  stopifnot( "days must both be in the format or mm-dd or pass just the year" =
               all(
                 grepl(pattern_wo_year, c(first.day, last.day)),
                 !is.null(year)
            ))

  if (grepl(pattern_wo_year, first.day)) {
    first.day = paste0(year, "-", first.day)
    last.day  = paste0(year, "-", last.day)
  } else {
    first.day = paste0(year, "-01-01")
    last.day  = paste0(year, "-12-31")
  }

  stopifnot( "last.day must be after first.day" =
               c(first.day, last.day) %>%
               {lubridate::ymd(.[2]) - lubridate::ymd(.[1])} %>%
               {as.integer(.) > 0 })

  list(first.day, last.day)
}


fiat_years = function(first.day, last.day) {
  dplyr::`%>%`

  years = lubridate::year(c(first.day, last.day)) %>%
    {seq(.[1], .[2])}

  x = list()
  for (year in as.character(years)) {
    x[[ year ]] = list(first.day = "01-01",
                       last.day  = "12-31")
    if (year == min(years)) x[[ year ]]$first.day = first.day |> substr(6, 10)
    if (year == max(years)) x[[ year ]]$last.day  = last.day  |> substr(6, 10)
  }

  x
}


local_data = function() {
  tools::R_user_dir("meteobr", which = "data") |>
    {\(.) gsub("\\\\", "/", .)}()
}


get_hash = function(file.arg) {
  import_rdata("R/info_raw_data.Rdata") |>
    dplyr::filter(file == file.arg) |>
    dplyr::select(hash) |>
    purrr::pluck(1) |>
    unname()
}


import_rdata = function(file) {
  env = new.env()
  load(file, envir = env)
  x = ls(env)
  get(x, envir = env)
}


inmet.args = list(
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
