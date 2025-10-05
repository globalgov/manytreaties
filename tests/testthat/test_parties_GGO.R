# Test if the dataset meets the many packages universe requirements

# Uniformity tests (agreements have a stateID and Begin columns)
test_that("datasets have the required variables", {
  expect_true("stateID" %in% names(parties[["GGO"]]))
  expect_true("Begin" %in% names(parties[["GGO"]]))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(parties[["GGO"]])))
  expect_false(any(lubridate::is.POSIXct(parties[["GGO"]])))
  expect_false(any(lubridate::is.POSIXlt(parties[["GGO"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(parties[["GGO"]]$Begin), "mdate")
  expect_false(any(grepl("/", parties[["GGO"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         parties[["GGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         parties[["GGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         parties[["GGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         parties[["GGO"]]$Begin)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by the `Begin` variable", {
  expect_true(parties[["GGO"]]$Begin[1] <
                parties[["GGO"]]$Begin[100])
  expect_true(parties[["GGO"]]$Begin[120] <
                parties[["GGO"]]$Begin[220])
  expect_true(parties[["GGO"]]$Begin[250] <
                parties[["GGO"]]$Begin[350])
})
