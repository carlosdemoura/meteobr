#' Get data from INMET website by year
#'
#' `get_inmet_data_by_year()` downloads data of a specific year from the
#' official INMET website and prepreprocess it.
#'
#' @param year                Integer number between 2000 and 2024.
#' @param first.day,last.day  String in the format "mm-dd". If NA (default), the
#'   first/last day of the year is considered.
#' @param vars                Variables to be collected. If NULL (default), all
#'   variables are collected.
#' @param stations            Stations to be collected. If NULL (default), all
#'   stations are collected.
#'
#' @return A `tibble()` containing data from all selected `stations` and `vars`
#'   in that `year` (or the part of it specified by `first.day` & `last.day`).
#'   Errors may arise if:
#' * `year` is not specified.
#' * You're trying to collect data before 2000-May-07.
#' * `fist.day` doesn't comes before `last.day`.
#' * Just one of `first.day` & `last.day` is passed.
#' * Your PC is not connected to the internet.
#'
#' @section First day in record:
#'   If you're trying to collect data of the year 2000 and the
#'   `first.day` is before 2000-May-07, you're only getting data from
#'   2000-May-07 and beyond because that was (allegedly) the day INMET automatic
#'   stations began operations. Note that some stations began collecting actual
#'   data some days after 2000-May-07.
#'
#' @examples
#' \dontrun{
#' # `first.day` & `last.day` must be in format mm-dd, here the code wouldn't run
#' # because it is in the format yyyy-mm-dd.
#' get_inmet_data_by_year(2000, first.day = "2000-01-01", last.day = "2000-12-31")
#'
#' # This Should get all 2000 data.
#' get_inmet_data_by_year(2000)
#' }
#'
#' @export
#'
#' @import dplyr
#' @import utils
#' @import purrr
#' @import tibble
#' @import stringr
#' @import lubridate
get_inmet_data_by_year = function(year, first.day = NA, last.day = NA, vars = NULL, stations = NULL) {

  csv.lines = validate_dates(year, first.day, last.day) |>
    {\(.) get_csv_lines(year, .[[1]], .[[2]])}()

  main_dir = file.path(tempdir(), paste0("meteobr_", year))

  "https://portal.inmet.gov.br/uploads/dadoshistoricos/" |>
    paste0(year, ".zip") |>
    utils::download.file(main_dir)

  extract_dir = file.path(tempdir(), "meteobr_unzipped")
  if (dir.exists(extract_dir)) unlink(extract_dir, recursive = T)
  dir.create(extract_dir)
  utils::unzip(main_dir, exdir = extract_dir)
  unlink(main_dir, recursive = T)

  if (year < 2020) {
    files = list.files(file.path(extract_dir, year), full.names = TRUE)
  } else {
    files = list.files(extract_dir, full.names = TRUE)
  }

  full_data = data.frame()

  for ( file in files ) {
    station = file    |>
      strsplit("_")   |>
      purrr::pluck(1) |>
      rev()           |>
      purrr::pluck(5)

    if (!(station %in% stations) & !is.null(stations)) {
      next
    }

    data = file |>
      utils::read.csv(skip = csv.lines[1], nrows = diff(csv.lines), sep = ";", header = F) |>
      dplyr::select(!dplyr::last_col()) |>
      `colnames<-` (c("day", "hour", "precipitation", "atm_pressure", "atm_pressure_max", "atm_pressure_min", "radiation", "temperature_air", "temperature_dew", "temperature_max", "temperature_min", "temperature_dew_max", "temperature_dew_min", "humidity_max", "humidity_min", "humidity", "wind_direction", "wind_burst_max", "wind_burst")
      ) |>
      tibble::as_tibble() |>
      {\(.) if (!is.null(vars)) dplyr::select(., all_of(c("day", "hour", vars))) else . }() |>
      dplyr::mutate(
        station = station,
        time = paste(day, hour) |>
          substr(1, 16)         |>
          lubridate::ymd_hm()   |>
          format("%Y/%m/%d %H")
      ) |>
      dplyr::select(!c(day, hour)) |>
      dplyr::mutate(
        dplyr::across(!all_of(c("station", "time")), \(x) stringr::str_replace(x, ",", ".")),
        dplyr::across(!all_of(c("station", "time")), \(x) as.numeric(x))
      ) |>
      dplyr::relocate(all_of(c("station", "time")))

    full_data = rbind(full_data, data)

  }

  unlink(extract_dir, recursive = T)

  full_data
}


#' Download preprocessed data
#'
#' `set_data_locally()` downloads .Rdata files and stores it locally without the
#' hassle of unzipping and processing them.
#'
#' @param years Vector of integers between 2000 and 2024; if no value is passed,
#'   the DEFAULT is to download all data.
#'
#' @return Logical.
#' * `TRUE`  if the required .Rdata is available in `local_data()`.
#' * `FALSE` if not.
#'
#' @examples
#' \dontrun{
#' # Should download all data available on
#' # <https://github.com/carlosdemoura/meteobr/raw/refs/heads/master/data/repo/>
#' # and store it where `local_data()` says.
#' set_data_locally()
#' }
#'
#' @export
#'
#' @import dplyr
#' @import utils
set_data_locally = function(years = 2000:2024) {
  stopifnot("year(s) must be between 2000 & 2004" =
              all(years %in% 2000:2024))


  if (!dir.exists(local_data())) dir.create(local_data(), recursive = T)

  cat("======== DOWNLOADING DATA ========\n")
  for (year in years) {
  info_repo = meteobr::info_repo |>
    {\(.) dplyr::filter(., .$type == "Rdata", .$year == !!year)}() |>
    as.data.frame()

  local_path = file.path(local_data(), paste0(year, ".Rdata"))

    if (all(file.exists(local_path),
            tools::md5sum(local_path) == info_repo[,"hash"] )) {
      cat(year, "\t", "Already available\t", "OK\n")
      next
    }

    url = paste0("https://github.com/carlosdemoura/meteobr/raw/refs/heads/master/data/repo/", year, ".Rdata")
    answer = try(utils::download.file(url, local_path, quiet = T))

    cat(year, "\t", round(info_repo[,"size"], 2), "Mb\t\t")
    if ( inherits(answer, "try-error") ) {
      #cat(year, "\t", info_repo[,"size"], "Mb\t\t\t", "ERROR\n")
      cat("ERROR\n")
    } else {
      #cat(year, "\t", info_repo[,"size"], "Mb\t\t\t", "OK\n")
      cat("OK\n")
    }

  }
}


#' Get data between dates
#'
#' `get_data()` does this. Beware that this function, check `local_data()` to
#' see more.
#'
#' @param first.day  String like "mm-dd".
#' @param last.day   String like "mm-dd".
#' @param vars       (optional) Variables to be collected.
#' @param stations   (optional) Stations to be collected.
#'
#' @return A `tibble()`.
#'
#' @examples
#' \dontrun{
#' # Should return `humidity` data from `station` A001 collected between 2019-april-02
#' # & 2022-november-08.
#' get_data(first.day = "2019-04-02", last.day = "2022-11-08", vars = "humidity", stations = "A001")
#' }
#'
#' @export
#'
#' @import dplyr
#' @import tools
#' @import tibble
#' @import lubridate
get_data = function(first.day, last.day, vars = NULL, stations = NULL) {
  years = fiat_years(first.day, last.day)

  full_data = data.frame()

  for (year in as.integer(names(years))) {
    set_data_locally(year)

    int = years[[as.character(year)]] |>
      {\(.) validate_dates(year, .$first.day, .$last.day)}() |>
      {\(.) lubridate::interval(lubridate::ymd(.[[1]]), lubridate::ymd(.[[2]]))}()

    data = paste0(local_data(), "/", year, ".Rdata") |>
      import_rdata() |>
      {\(.)
        if (lubridate::int_length(int) / 86400 < 364)
        dplyr::filter(., lubridate::ymd_h(.data$time) %within% int)
        else . }() |>
      {\(.)
        if (!is.null(stations))
        dplyr::filter(., .$station %in% stations)
        else . }() |>
      {\(.)
        if (!is.null(vars))
        dplyr::select(., all_of(c("time", vars)))
        else . }()

    full_data = rbind(full_data, data)
  }

  full_data
}
