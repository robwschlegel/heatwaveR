context("Test exceedance.R")

test_that("exceedance() returns the correct lists, tibbles, and columns", {
  res <- exceedance(data = make_whole(sst_Med), threshold = 20)
  expect_is(res, "list")
  expect_is(res$threshold, "tbl_df")
  expect_is(res$exceedance, "data.frame")
  expect_equal(ncol(res$threshold), 7)
  expect_equal(ncol(res$exceedance), 17)
})
# Need to fix exceedance not needing a threshold...
# Fix res$exceedance to be a tibble output
