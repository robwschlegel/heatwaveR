context("Test smooth_percentile.R")

test_that("smooth_percentile() returns the correct output", {
  ts_xy <- sst_Med
  colnames(ts_xy) <- c("ts_x", "ts_y")
  ts_whole <- heatwaveR:::make_whole_fast(ts_xy)
  ts_wide <- heatwaveR:::clim_spread(ts_whole, clim_start = "1983-01-01",
                                     clim_end = "2012-12-31", windowHalfWidth = 5)
  ts_calc <- heatwaveR:::clim_calc(ts_wide, windowHalfWidth = 5, pctile = 90)
  res <- heatwaveR:::smooth_percentile(ts_calc, smoothPercentileWidth = 31, var_calc = F)
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 3)
  expect_equal(nrow(res), 366)
})

test_that("a variance column is created if desired", {
  ts_xy <- sst_Med
  colnames(ts_xy) <- c("ts_x", "ts_y")
  ts_whole <- heatwaveR:::make_whole_fast(ts_xy)
  ts_wide <- heatwaveR:::clim_spread(ts_whole, clim_start = "1983-01-01",
                                     clim_end = "2012-12-31", windowHalfWidth = 5)
  ts_calc <- heatwaveR:::clim_calc(ts_wide, windowHalfWidth = 5, pctile = 90)
  res <- heatwaveR:::smooth_percentile(ts_calc, smoothPercentileWidth = 31, var_calc = T)
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 4)
  expect_equal(nrow(res), 366)
})
