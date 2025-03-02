get_csv_lines = function(year, first.day, last.day) {
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
      stop("There was no data collected before 2000-May-07")
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


validate_dates = function(year, first.day, last.day) {
  stopifnot( "only data between 2000 & 2024 is available" = year %in% 2000:2024 )

  pattern_wo_year = "^\\d{2}[-/]\\d{2}$"

  stopifnot( "days must both be in the format or mm-dd or pass just the year" =
               all(
                 grepl(pattern_wo_year, c(first.day, last.day)) | all(is.na(c(first.day, last.day))),
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
               c(first.day, last.day) |>
               {\(.) lubridate::ymd(.[2]) - lubridate::ymd(.[1])}() |>
               {\(.) as.integer(.) > 0 }() )

  list(first.day, last.day)
}


fiat_years = function(first.day, last.day) {
  years = lubridate::year(c(first.day, last.day)) |>
    {\(.) seq(.[1], .[2])}()

  x = list()
  for (year in as.character(years)) {
    x[[ year ]] = list(first.day = "01-01",
                       last.day  = "12-31")
    if (year == min(years)) x[[ year ]]$first.day = first.day |> substr(6, 10)
    if (year == max(years)) x[[ year ]]$last.day  = last.day  |> substr(6, 10)
  }

  x
}


get_hash = function(file.arg) {
  import_rdata("R/info_raw_data.Rdata") |>
    dplyr::filter(file == file.arg) |>
    dplyr::select(hash) |>
    purrr::pluck(1) |>
    unname()
}


#' Get data
#'
#' @export
import_rdata = function(file) {
  env = new.env()
  load(file, envir = env)
  x = ls(env)
  get(x, envir = env)
}


#' Get where data is stored
#'
#' Get where on your PC the package data is being stored.
#'
#' @return A string, folder path.
#' @export
local_data = function() {
  tools::R_user_dir("meteobr", which = "data") |>
    {\(.) gsub("\\\\", "/", .)}()
}
