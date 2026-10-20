context("Test clim_calc.R")

test_that("clim_calc() returns the correct output", {
  ts_xy <- sst_Med
  colnames(ts_xy) <- c("ts_x", "ts_y")
  ts_whole <- heatwaveR:::make_whole_fast(ts_xy)
  ts_wide <- heatwaveR:::clim_spread(ts_whole, clim_start = "1983-01-01",
                                     clim_end = "2012-12-31", windowHalfWidth = 5)
  res <- heatwaveR:::clim_calc(ts_wide, windowHalfWidth = 5, pctile = 90)
  expect_is(res, "matrix")
  expect_equal(ncol(res), 4)
  expect_equal(nrow(res), 366)
})

test_that("clim_calc_cpp() handles pctile = 100 and windows without values", {
  ts_xy <- sst_Med
  colnames(ts_xy) <- c("ts_x", "ts_y")
  ts_whole <- heatwaveR:::make_whole_fast(ts_xy)
  ts_wide <- heatwaveR:::clim_spread(ts_whole, clim_start = "1983-01-01",
                                     clim_end = "2012-12-31", windowHalfWidth = 5)
  res_r <- heatwaveR:::clim_calc(ts_wide, windowHalfWidth = 5, pctile = 100)
  res_cpp <- heatwaveR:::clim_calc_cpp(ts_wide, windowHalfWidth = 5, pctile = 100)
  expect_equal(res_cpp[, "thresh"], res_r[, "thresh"])
  ts_wide[1:20, ] <- NA
  res_cpp <- heatwaveR:::clim_calc_cpp(ts_wide, windowHalfWidth = 5, pctile = 90)
  expect_true(all(is.nan(res_cpp[1:10, "thresh"])))
  expect_false(anyNA(res_cpp[16:366, "thresh"]))
})
