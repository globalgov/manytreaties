# Test generate_agreements()

test_that("length of random list is correct", {
  expect_length(generate_agreements(12), 12)
  expect_equal(class(generate_agreements(1)), "character")
  set.seed(1234)
  expect_length(unique(generate_agreements(12)), 12)
  # expect_length(generate_memberships(12, list = TRUE)[,1], 12)
  # expect_equal(class(generate_memberships(2)[,2]), "character")
  # set.seed(1234)
  # expect_length(unique(generate_memberships(2)[,1]), 2)
})
