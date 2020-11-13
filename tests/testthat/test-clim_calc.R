context("Test clim_calc.R")

test_that("clim_calc() returns the correct output", {
  ts_xy <- sst_Med
  colnames(ts_xy) <- c("ts_x", "ts_y")
  ts_whole <- heatwaveR:::make_whole(ts_xy, x = ts_x, y = ts_y)
  # ts_whole$ts_y <- zoo::na.approx(ts_whole$ts_y, maxgap = 3)
  ts_wide <- heatwaveR:::clim_spread(ts_whole, clim_start = "1983-01-01",
                                     clim_end = "2012-12-31", windowHalfWidth = 5)
  res <- heatwaveR:::clim_calc(ts_wide, windowHalfWidth = 5, pctile = 90)
  expect_is(res, "matrix")
  expect_equal(ncol(res), 4)
  expect_equal(nrow(res), 366)
})
