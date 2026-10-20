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
  expect_false(anyNA(res[6:371, ]))
})

test_that("clim_spread() pads with the neighbouring days of the adjacent years", {
  ts_xy <- sst_Med
  colnames(ts_xy) <- c("ts_x", "ts_y")
  ts_whole <- heatwaveR:::make_whole_fast(ts_xy)
  res <- heatwaveR:::clim_spread(ts_whole, clim_start = "1983-01-01",
                                 clim_end = "2012-12-31", windowHalfWidth = 5)
  # before 1 January: 27-31 December of the previous year
  expect_equal(unname(res[1:5, 2:30]), unname(res[367:371, 1:29]))
  # after 31 December: 1-5 January of the next year
  expect_equal(unname(res[372:376, 1:29]), unname(res[6:10, 2:30]))
  # none before the first or after the last year of the climatology period
  expect_true(all(is.na(res[1:5, 1])))
  expect_true(all(is.na(res[372:376, 30])))
  expect_false(anyNA(res[1:5, 2:30]))
  expect_false(anyNA(res[372:376, 1:29]))
})
