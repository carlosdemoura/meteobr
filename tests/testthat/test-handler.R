test_that("get_inmet_data_by_year(), set_data_locally(), get_data() validate year", {
  expect_error(get_inmet_data_by_year(2000, first.day = "01-01", last.day = "02-01"))
  expect_error(get_inmet_data_by_year(1999))
  expect_error(set_data_locally(1999))
  expect_error(set_data_locally(c(1999:2024)))
  expect_error(get_data(first.day = "1999-12-31", last.day = "2001-12-31"))
})

# test_that("get_inmet_data_by_year() works", {
#   skip_on_cran()
#   actual = get_inmet_data_by_year(2000)
#
#   x = new.env()
#   load(test_path("fixtures", "data_2000.Rdata"), envir = x)
#   expected = get(ls(x))
#
#   expect_equal(actual, expected)
# })

test_that("set_data_locally() works", {
  set_data_locally(2000)
  expect_true( file.exists(file.path(local_data(), "2000.Rdata")) )
})

test_that("get_data() works", {
  actual = get_data(first.day = "2000-01-01", last.day = "2000-12-31")
  expected = readRDS(test_path("fixtures", "data_2000.rds"))
  expect_equal(actual, expected)
})
