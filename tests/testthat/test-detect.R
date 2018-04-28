context("Test detect.R")

test_that("detect() returns the correct lists, tibbles, and columns", {
  res <- detect(data = make_whole(sst_Med),
                climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  expect_is(res, "list")
  expect_is(res$clim, "tbl_df")
  expect_is(res$event, "tbl_df")
  expect_equal(ncol(res$clim), 10)
  expect_equal(ncol(res$event), 23)
})
