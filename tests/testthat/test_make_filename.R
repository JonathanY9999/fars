context("make_filename")

test_that("string is created correctly", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})
