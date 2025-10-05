# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", parties[["GPTAD"]])))
  expect_false(any(grepl("^N/A$", parties[["GPTAD"]])))
  expect_false(any(grepl("^\\s$", parties[["GPTAD"]])))
  expect_false(any(grepl("^\\.$", parties[["GPTAD"]])))
  expect_false(any(grepl("N\\.A\\.$", parties[["GPTAD"]])))
  expect_false(any(grepl("n\\.a\\.$", parties[["GPTAD"]])))
})

# Uniformity tests (agreements have a stateID and Begin columns)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(parties[["GPTAD"]],
                                pointblank::vars(stateID))
  pointblank::expect_col_exists(parties[["GPTAD"]],
                                pointblank::vars(Begin))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(parties[["GPTAD"]])))
  expect_false(any(lubridate::is.POSIXct(parties[["GPTAD"]])))
  expect_false(any(lubridate::is.POSIXlt(parties[["GPTAD"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(parties[["GPTAD"]]$Begin), "mdate")
  expect_false(any(grepl("/", parties[["GPTAD"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         parties[["GPTAD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         parties[["GPTAD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         parties[["GPTAD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         parties[["GPTAD"]]$Begin)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by the `Begin` variable", {
  expect_true(parties[["GPTAD"]]$Begin[1] <
                parties[["GPTAD"]]$Begin[100])
  expect_true(parties[["GPTAD"]]$Begin[120] <
                parties[["GPTAD"]]$Begin[220])
  expect_true(parties[["GPTAD"]]$Begin[250] <
                parties[["GPTAD"]]$Begin[350])
})
