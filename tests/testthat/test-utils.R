test_that("get_csv_lines() works", {
  expect_error(get_csv_lines(2000, "2000-01-01", "2000-02-02"))
  expect_equal(get_csv_lines(2000, "2000-01-01", "2000-05-07"), c(9, 33))
  expect_equal(get_csv_lines(2001, NA, NA), c(9, 8769))
})


test_that("validate_dates() works", {
  expect_error(validate_dates(1999))
  expect_error(validate_dates(NULL, NA, NA))
  expect_error(validate_dates(2000, "2000-01-01", "2000-02-01"))
  expect_equal(validate_dates(2000), list("2000-01-01", "2000-12-31"))
  expect_equal(validate_dates(2000, "01-01", "02-01"), list("2000-01-01", "2000-02-01"))
})


test_that("fiat_years() works", {
  expect_equal(fiat_years("2000-06-01", "2000-08-02"),
               list("2000" = list(first.day = "06-01",
                                  last.day  = "08-02")))

  expect_equal(fiat_years("2000-04-05", "2002-08-22"),
               list("2000" = list(first.day = "04-05",
                                  last.day  = "12-31"),
                    "2001" = list(first.day = "01-01",
                                  last.day  = "12-31"),
                    "2002" = list(first.day = "01-01",
                                  last.day  = "08-22")))
})
