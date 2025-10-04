# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["TFDD"]])))
  expect_false(any(grepl("^N/A$", agreements[["TFDD"]])))
  expect_false(any(grepl("^\\s$", agreements[["TFDD"]])))
  expect_false(any(grepl("^\\.$", agreements[["TFDD"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["TFDD"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["TFDD"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["TFDD"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists(agreements[["TFDD"]],
                                pointblank::vars(Begin))
  expect_true(any(grepl("ID$", colnames(agreements[["TFDD"]]))))
  pointblank::expect_col_exists(agreements[["TFDD"]],
                                pointblank::vars(Signature))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["TFDD"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["TFDD"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["TFDD"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(agreements[["TFDD"]]$Begin), "mdate")
  expect_false(any(grepl("/", agreements[["TFDD"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["TFDD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["TFDD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["TFDD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["TFDD"]]$Begin)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["TFDD"]]$Signature), "mdate")
  expect_false(any(grepl("/", agreements[["TFDD"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["TFDD"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["TFDD"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["TFDD"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["TFDD"]]$Signature)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["TFDD"]]$Begin[1] <
                agreements[["TFDD"]]$Begin[10])
  expect_true(agreements[["TFDD"]]$Begin[50] <
                agreements[["TFDD"]]$Begin[75])
  expect_true(agreements[["TFDD"]]$Begin[100] <
                agreements[["TFDD"]]$Begin[120])
})
