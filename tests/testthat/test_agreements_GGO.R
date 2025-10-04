# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["GGO"]])))
  expect_false(any(grepl("^N/A$", agreements[["GGO"]])))
  expect_false(any(grepl("^\\s$", agreements[["GGO"]])))
  expect_false(any(grepl("^\\.$", agreements[["GGO"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["GGO"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["GGO"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["GGO"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists(agreements[["GGO"]],
                                pointblank::vars(Begin))
  expect_true(any(grepl("ID$", colnames(agreements[["GGO"]]))))
  pointblank::expect_col_exists(agreements[["GGO"]],
                                pointblank::vars(Signature))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["GGO"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["GGO"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["GGO"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(agreements[["GGO"]]$Begin), "mdate")
  expect_false(any(grepl("/", agreements[["GGO"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["GGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["GGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["GGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["GGO"]]$Begin)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["GGO"]]$Signature), "mdate")
  expect_false(any(grepl("/", agreements[["GGO"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["GGO"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["GGO"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["GGO"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["GGO"]]$Signature)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["GGO"]]$Begin[1] <
                agreements[["GGO"]]$Begin[10])
  expect_true(agreements[["GGO"]]$Begin[50] <
                agreements[["GGO"]]$Begin[75])
  expect_true(agreements[["GGO"]]$Begin[100] <
                agreements[["GGO"]]$Begin[120])
})
