
get_inmet_raw_data_year = function(year, first.day = NA, last.day = NA, vars = NULL, stations = NULL, local.path = NULL) {

  csv.lines = validate_dates(year, first.day, last.day) %>%
    {adjust_lines_csv(year, .[[1]], .[[2]])}

  # days = validate_dates(year, first.day, last.day)
  # first.day = days[[1]]
  # last.day  = days[[2]]
  # csv.lines = adjust_lines_csv(days)

  stations = drop_stations(stations)

  if (is.null(local.path)) {
    main_dir = file.path(tempdir(), paste0("inmet", year))

    "https://portal.inmet.gov.br/uploads/dadoshistoricos/" |>
      paste0(year, ".zip") |>
      download.file(main_dir)
  } else {
    main_dir = local.path
  }

  extract_dir = file.path(tempdir(), "data")

  if (!dir.exists(extract_dir)) dir.create(extract_dir)

  unzip(main_dir, exdir = extract_dir)

  if (year < 2020) {
    files = list.files(file.path(extract_dir, year), full.names = TRUE)
  } else {
    files = list.files(extract_dir, full.names = TRUE)
  }

  dados_tratados = data.frame()

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
        dplyr::across(!c(station, time), stringr::str_replace, ",", "."),
        dplyr::across(!c(station, time), as.numeric)
      ) %>%
      dplyr::relocate(c(station, time))

    dados_tratados = rbind(dados_tratados, data)
  }

  unlink(main_dir)
  unlink(extract_dir)

  dados_tratados
}


get_github_raw_data_year = function(year, first.day, last.day, vars, stations) {

  int = validate_dates(year, first.day, last.day) %>%
    {lubridate::interval(lubridate::ymd(.[[1]]), lubridate::ymd(.[[2]]))}

  stations = drop_stations(stations)

  data = year %>%
    {paste0("C:/Users/Carlos/Downloads/anos/br_", ., ".fst")} %>%
    fst::fst() %>%
    as_tibble() %>%
    filter( lubridate::ymd_h(time) %within% int )
}


get_data = function(first.day, last.day, vars = NULL, stations = NULL, type, source) {
  years = fiat_years(first.day, last.day)

  full_data = data.frame()

  for (year in as.integer(names(years))) {
    first.day = years[[as.character(year)]]$first.day
    last.day = years[[as.character(year)]]$last.day

    if (type == "inmet") {
      data = get_inmet_raw_data_year(year, first.day, last.day, vars, stations)
    } else if (type == "github") {
      data = get_github_raw_data_year(year, first.day, last.day, vars, stations)
    }

    full_data = rbind(full_data, data)
  }

  full_data
}


set_data_locally = function(year, font = "github", path = NULL) {
  stopifnot("font must be either 'github' or 'inmet'" =
              font %in% c("github", "inmet"))

  if (is.null(path)) {
    path = tools::R_user_dir("meteobr", which = "data") |>
      {\(.) gsub("\\\\", "/", .)}()
  }

  file = year |>
    paste0(ifelse(font == "github", ".fst", ".zip"))

  if ( file.exists(file.path(path, file)) ) {
    if ( tools::md5sum(file.path(path, file)) == verify_hash(file) ) {
      return("file already available locally")
    } else {
      stop(paste0("a different file with the same name is where the local file should be, you must manually clean ", file.path(path, file)))
    }
  }

  if (font == "inmet") {
    "https://portal.inmet.gov.br/uploads/dadoshistoricos/" |>
      paste0(file) |>
      download.file(file.path(path, file))
  } else if (font == "github") {
    "https://github.com/carlosdemoura/meteobr/tree/master/data/" |>
      paste0(file) |>
      download.file(file.path(path, file))

  }


}
