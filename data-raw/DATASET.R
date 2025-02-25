## code to prepare `DATASET` dataset goes here

# for (year in 2000:2024) {
#   set_data_locally_year(year, font = "inmet", quiet = F)
#   fst::write_fst(get_inmet_raw_data_year(year, local.path = local_data()),
#                  paste0(local_data(), "/", year, ".fst"),
#                  compress = 100)
#   file = paste0("br_", year)
#   assign(file, fst::fst(paste0(local_data(), "/", year, ".fst")) |> as_tibble())
#   save(file = paste0(local_data(), "/", year, ".Rdata"),
#        list = file,
#        compress = "bzip2")
#   rm(file)
#   print(year)
# }


info_raw_data = data.frame(path = list.files(local_data(), full.names = TRUE)) |>
  dplyr::mutate(
    size = file.info(path)$size,
    hash = sapply(path,
                  function(x) {
                    tools::md5sum(x) |> unname()
                  }),
    file = sapply(path,
                  function(x) {
                    strsplit(x, "/") |>
                      purrr::pluck(1) |>
                      rev() |>
                      purrr::pluck(1)
                  }),
    year = sapply(file,
                  function(x) {
                    strsplit(x, "[.]") |>
                      purrr::pluck(1) |>
                      list()
                  }),
    type = sapply(year,
                  function(x) {
                    x |> purrr::pluck(2)
                  }),
    year = sapply(year,
                  function(x) {
                    x |> purrr::pluck(1) |> as.numeric()
                  })
  ) |>
  dplyr::select(!path) |>
  dplyr::relocate(c(year, type, file)) |>
  tibble::as_tibble()

save(info_raw_data, file = "R/info_raw_data.Rdata")

#usethis::use_data(DATASET, overwrite = TRUE)
