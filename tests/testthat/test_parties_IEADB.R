# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", parties[["IEADB"]])))
  expect_false(any(grepl("^N/A$", parties[["IEADB"]])))
  expect_false(any(grepl("^\\s$", parties[["IEADB"]])))
  expect_false(any(grepl("^\\.$", parties[["IEADB"]])))
  expect_false(any(grepl("N\\.A\\.$", parties[["IEADB"]])))
  expect_false(any(grepl("n\\.a\\.$", parties[["IEADB"]])))
})

# Uniformity tests (agreements have a stateID and Begin columns)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(parties[["IEADB"]],
                                pointblank::vars(stateID))
  pointblank::expect_col_exists(parties[["IEADB"]],
                                pointblank::vars(Begin))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(parties[["IEADB"]])))
  expect_false(any(lubridate::is.POSIXct(parties[["IEADB"]])))
  expect_false(any(lubridate::is.POSIXlt(parties[["IEADB"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(parties[["IEADB"]]$Begin), "mdate")
  expect_false(any(grepl("/", parties[["IEADB"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         parties[["IEADB"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         parties[["IEADB"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         parties[["IEADB"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         parties[["IEADB"]]$Begin)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by the `Begin` variable", {
  expect_true(parties[["IEADB"]]$Begin[1] <
                parties[["IEADB"]]$Begin[100])
  expect_true(parties[["IEADB"]]$Begin[120] <
                parties[["IEADB"]]$Begin[220])
  expect_true(parties[["IEADB"]]$Begin[250] <
                parties[["IEADB"]]$Begin[350])
})
