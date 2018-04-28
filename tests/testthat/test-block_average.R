context("Test block_average.R")

test_that("block_average() returns the correct tibble and columns", {
  res1 <- detect(data = make_whole(sst_Med),
                 climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  res2 <- block_average(res1)
  expect_is(res2, "tbl_df")
  expect_equal(ncol(res2), 23)
})
