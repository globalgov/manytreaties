# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", parties[["DESTA"]])))
  expect_false(any(grepl("^N/A$", parties[["DESTA"]])))
  expect_false(any(grepl("^\\s$", parties[["DESTA"]])))
  expect_false(any(grepl("^\\.$", parties[["DESTA"]])))
  expect_false(any(grepl("N\\.A\\.$", parties[["DESTA"]])))
  expect_false(any(grepl("n\\.a\\.$", parties[["DESTA"]])))
})

# Uniformity tests (agreements have a stateID and Begin columns)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(parties[["DESTA"]],
                                pointblank::vars(stateID))
  pointblank::expect_col_exists(parties[["DESTA"]],
                                pointblank::vars(Begin))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(parties[["DESTA"]])))
  expect_false(any(lubridate::is.POSIXct(parties[["DESTA"]])))
  expect_false(any(lubridate::is.POSIXlt(parties[["DESTA"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(parties[["DESTA"]]$Begin), "mdate")
  expect_false(any(grepl("/", parties[["DESTA"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         parties[["DESTA"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         parties[["DESTA"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         parties[["DESTA"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         parties[["DESTA"]]$Begin)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by the `Begin` variable", {
  expect_true(parties[["DESTA"]]$Begin[1] <
                parties[["DESTA"]]$Begin[100])
  expect_true(parties[["DESTA"]]$Begin[120] <
                parties[["DESTA"]]$Begin[220])
  expect_true(parties[["DESTA"]]$Begin[250] <
                parties[["DESTA"]]$Begin[350])
})
