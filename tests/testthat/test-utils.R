test_that("get_csv_lines() works", {
  expect_equal(2 * 2, 4)
})


test_that("validate_dates() works", {
  expect_error(NULL, NA, NA)
  expect_equal(2 * 2, 4)
})


test_that("fiat_years() works", {
  expect_equal(fiat_years("2000-06-01", "2000-08-02"), list("2000" = list(first.day = "06-01",
                                                                          last.day  = "08-02")
                                                            ))
  expect_equal(fiat_years("2000-04-05", "2002-08-22"), list("2000" = list(first.day = "04-05",
                                                                          last.day  = "12-31"),
                                                            "2001" = list(first.day = "01-01",
                                                                          last.day  = "12-31"),
                                                            "2002" = list(first.day = "01-01",
                                                                          last.day  = "08-22")
                                                            ))
})
