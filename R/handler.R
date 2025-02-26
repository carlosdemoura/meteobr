get_inmet_data_by_year = function(year, first.day = NA, last.day = NA, vars = NULL, stations = NULL, local.path = local_data()) {

  csv.lines = validate_dates(year, first.day, last.day) %>%
    {adjust_lines_csv(year, .[[1]], .[[2]])}

  main_dir = file.path(tempdir(), paste0("meteobr_", year))

  "https://portal.inmet.gov.br/uploads/dadoshistoricos/" |>
    paste0(year, ".zip") |>
    download.file(main_dir)

  extract_dir = file.path(tempdir(), "meteobr_unzipped")
  if (dir.exists(extract_dir)) unlink(extract_dir, recursive = T)
  dir.create(extract_dir)
  unzip(main_dir, exdir = extract_dir)
  unlink(main_dir, recursive = T)

  if (year < 2020) {
    files = list.files(file.path(extract_dir, year), full.names = TRUE)
  } else {
    files = list.files(extract_dir, full.names = TRUE)
  }

  full_data = data.frame()

  for ( file in files ) {
    station = file |>
      strsplit("_") |>
      purrr::pluck(1) |>
      rev() |>
      purrr::pluck(5)

    if (!(station %in% stations) & !is.null(stations)) {
      next
    }

    data = file %>%
      read.csv(skip = csv.lines[1], nrows = diff(csv.lines), sep = ";", header = F) %>%
      dplyr::select(!last_col()) %>%
      `colnames<-` (c("day", "hour", "precipitation", "atm_pressure", "atm_pressure_max", "atm_pressure_min", "radiation", "temperature_air", "temperature_dew", "temperature_max", "temperature_min", "temperature_dew_max", "temperature_dew_min", "humidity_max", "humidity_min", "humidity", "wind_direction", "wind_burst_max", "wind_burst")
      ) %>%
      tibble::as_tibble() %>%
      { if (!is.null(vars)) select(., all_of(c("day", "hour", vars))) else . } %>%
      dplyr::mutate(
        station = station,
        time = paste(day, hour) %>%
          substr(1, 16) %>%
          lubridate::ymd_hm() %>%
          format("%Y/%m/%d %H")
      ) %>%
      dplyr::select(!c(day, hour)) %>%
      dplyr::mutate(
        dplyr::across(!c(station, time), \(x) stringr::str_replace(x, ",", ".")),
        dplyr::across(!c(station, time), \(x) as.numeric(x))
      ) %>%
      dplyr::relocate(c(station, time))

    full_data = rbind(full_data, data)

  }

  unlink(extract_dir, recursive = T)

  full_data
}


set_data_locally = function(years = 2000:2024) {
  stopifnot("year(s) must be between 2000 & 2004" =
              all(years %in% 2000:2024))

  for (year in years) {
    "https://github.com/carlosdemoura/meteobr/raw/refs/heads/master/data/" |>
      paste0(year, ".Rdata") |>
      download.file(paste0(local_data(), "/", year, ".Rdata"))
  }
}


get_data = function(first.day, last.day, vars = NULL, stations = NULL) {
  years = fiat_years(first.day, last.day)

  full_data = data.frame()

  for (year in as.integer(names(years))) {
    file.path = paste0(local_data(), "/", year, ".Rdata")

    if(
      any(
        !file.exists(file.path),
        tools::md5sum(file.path) != get_hash(paste0(year, ".Rdata"))
      )) { set_data_locally(year) }

    int = years[[as.character(year)]] %>%
      {validate_dates(year, .$first.day, .$last.day)} %>%
      {lubridate::interval(lubridate::ymd(.[[1]]), lubridate::ymd(.[[2]]))}

    data = file.path %>%
      import_rdata() %>%
      { if (lubridate::int_length(int) / 86400 < 364)
        dplyr::filter(., lubridate::ymd_h(.data$time) %within% int)
        else . } %>%
      { if (!is.null(stations))
        dplyr::filter(., .data$station %in% stations)
        else . } %>%
      { if (!is.null(vars))
        dplyr::select(., all_of(c("time", vars)))
        else . }

    full_data = rbind(full_data, data)
  }

  full_data
}
