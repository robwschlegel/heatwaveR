context("Test block_average.R")

test_that("block_average() returns the correct tibble and columns", {
  res1 <- detect(data = make_whole(sst_Med),
                 climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  res2 <- block_average(res1)
  expect_is(res2, "tbl_df")
  expect_equal(ncol(res2), 23)
})

test_that("block_average() report argument behaves as expected", {
  res <- detect(data = make_whole(sst_Med),
                 climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  res_full <- block_average(res, report = "full")
  res_partial <- block_average(res, report = "partial")
  expect_is(res_full, "tbl_df")
  expect_is(res_partial, "tbl_df")
  expect_error(block_average(res, report = "part"), "Oops, 'report' must be either 'full' or 'partial'!")
})
