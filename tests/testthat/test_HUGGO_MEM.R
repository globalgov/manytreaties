# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", parties[["HUGGO"]])))
  expect_false(any(grepl("^N/A$", parties[["HUGGO"]])))
  expect_false(any(grepl("^\\s$", parties[["HUGGO"]])))
  expect_false(any(grepl("^\\.$", parties[["HUGGO"]])))
  expect_false(any(grepl("N\\.A\\.$", parties[["HUGGO"]])))
  expect_false(any(grepl("n\\.a\\.$", parties[["HUGGO"]])))
})

# Uniformity tests (agreements have a stateID and Begin columns)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(parties[["HUGGO"]],
                                pointblank::vars(stateID))
  pointblank::expect_col_exists(parties[["HUGGO"]],
                                pointblank::vars(Begin))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(parties[["HUGGO"]])))
  expect_false(any(lubridate::is.POSIXct(parties[["HUGGO"]])))
  expect_false(any(lubridate::is.POSIXlt(parties[["HUGGO"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(parties[["HUGGO"]]$Begin), "mdate")
  expect_false(any(grepl("/", parties[["HUGGO"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         parties[["HUGGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         parties[["HUGGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         parties[["HUGGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         parties[["HUGGO"]]$Begin)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by the `Begin` variable", {
  expect_true(parties[["HUGGO"]]$Begin[1] <
                parties[["HUGGO"]]$Begin[100])
  expect_true(parties[["HUGGO"]]$Begin[120] <
                parties[["HUGGO"]]$Begin[220])
  expect_true(parties[["HUGGO"]]$Begin[250] <
                parties[["HUGGO"]]$Begin[350])
})
