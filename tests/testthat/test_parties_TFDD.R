# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", parties[["TFDD"]])))
  expect_false(any(grepl("^N/A$", parties[["TFDD"]])))
  expect_false(any(grepl("^\\s$", parties[["TFDD"]])))
  expect_false(any(grepl("^\\.$", parties[["TFDD"]])))
  expect_false(any(grepl("N\\.A\\.$", parties[["TFDD"]])))
  expect_false(any(grepl("n\\.a\\.$", parties[["TFDD"]])))
})

# Uniformity tests (agreements have a stateID and Begin columns)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(parties[["TFDD"]],
                                pointblank::vars(stateID))
  pointblank::expect_col_exists(parties[["TFDD"]],
                                pointblank::vars(Begin))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(parties[["TFDD"]])))
  expect_false(any(lubridate::is.POSIXct(parties[["TFDD"]])))
  expect_false(any(lubridate::is.POSIXlt(parties[["TFDD"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(parties[["TFDD"]]$Begin), "mdate")
  expect_false(any(grepl("/", parties[["TFDD"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         parties[["TFDD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         parties[["TFDD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         parties[["TFDD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         parties[["TFDD"]]$Begin)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by the `Begin` variable", {
  expect_true(parties[["TFDD"]]$Begin[1] <
                parties[["TFDD"]]$Begin[100])
  expect_true(parties[["TFDD"]]$Begin[120] <
                parties[["TFDD"]]$Begin[220])
  expect_true(parties[["TFDD"]]$Begin[250] <
                parties[["TFDD"]]$Begin[350])
})
