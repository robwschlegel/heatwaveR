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

test_that("clim_calc() and clim_calc_cpp() agree, skipping the padding without neighbours", {
  ts_xy <- sst_Med
  colnames(ts_xy) <- c("ts_x", "ts_y")
  ts_whole <- heatwaveR:::make_whole_fast(ts_xy)
  ts_wide <- heatwaveR:::clim_spread(ts_whole, clim_start = "1983-01-01",
                                     clim_end = "2012-12-31", windowHalfWidth = 5)
  res_r <- heatwaveR:::clim_calc(ts_wide, windowHalfWidth = 5, pctile = 90)
  res_cpp <- heatwaveR:::clim_calc_cpp(ts_wide, windowHalfWidth = 5, pctile = 90)
  expect_false(anyNA(res_cpp))
  expect_equal(res_cpp[, "seas"], res_r[, "seas"])
  expect_equal(res_cpp[, "thresh"], res_r[, "thresh"])
})

test_that("each day's window holds its real neighbouring days, within the climatology period", {
  ts_xy <- sst_Med
  colnames(ts_xy) <- c("ts_x", "ts_y")
  ts_whole <- heatwaveR:::make_whole_fast(ts_xy)
  ts_wide <- heatwaveR:::clim_spread(ts_whole, clim_start = "1983-01-01",
                                     clim_end = "2012-12-31", windowHalfWidth = 5)
  # the 30 years end to end, in time order (366 days per year, as spread)
  series <- c(ts_wide[6:371, ])
  ref <- t(sapply(1:366, function(d) {
    idx <- outer(d + 366 * (0:29), -5:5, "+")
    v <- series[idx[idx >= 1 & idx <= length(series)]]
    c(mean(v), stats::quantile(v, 0.9, type = 7, names = FALSE))
  }))
  res <- heatwaveR:::clim_calc_cpp(ts_wide, windowHalfWidth = 5, pctile = 90)
  expect_equal(unname(res[, "seas"]), ref[, 1])
  expect_equal(unname(res[, "thresh"]), ref[, 2])
})
