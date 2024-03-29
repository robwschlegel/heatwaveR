context("Test clim_spread.R")

test_that("clim_spread() returns the correct output", {
  ts_xy <- sst_Med
  colnames(ts_xy) <- c("ts_x", "ts_y")
  ts_whole <- heatwaveR:::make_whole_fast(ts_xy)
  res <- heatwaveR:::clim_spread(ts_whole, clim_start = "1983-01-01",
                                 clim_end = "2012-12-31", windowHalfWidth = 5)
  expect_is(res, "matrix")
  expect_equal(ncol(res), 30)
  expect_equal(nrow(res), 376)
})

test_that("clim_spread() plugs NA values", {
  ts_xy <- sst_Med
  colnames(ts_xy) <- c("ts_x", "ts_y")
  ts_whole <- heatwaveR:::make_whole_fast(ts_xy)
  res <- heatwaveR:::clim_spread(ts_whole, clim_start = "1983-02-01",
                                 clim_end = "2012-01-31", windowHalfWidth = 5)
  expect_is(res, "matrix")
  expect_equal(nrow(na.omit(res)), 376)
})
