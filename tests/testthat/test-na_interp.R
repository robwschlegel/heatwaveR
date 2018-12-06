context("Test na_interp.R")

test_that("na_interp() doesn't fall over", {
  sst_Med_prep <- sst_Med %>%
    dplyr::rename(ts_x = t, ts_y = temp)
  ts <- heatwaveR:::make_whole_fast(sst_Med_prep)
  res <- heatwaveR:::na_interp(doy = ts$doy, x = ts$ts_x, y = ts$ts_y, maxPadLength = 2)
  expect_is(res, "data.table")
})

test_that("na_interp() handles unusual maxPadLength values correctly", {
  sst_Med_prep <- sst_Med %>%
    dplyr::rename(ts_x = t, ts_y = temp)
  ts <- heatwaveR:::make_whole_fast(sst_Med_prep)
  expect_is(heatwaveR:::na_interp(doy = ts$doy, x = ts$ts_x, y = ts$ts_y, maxPadLength = 0), "data.table")
  expect_is(heatwaveR:::na_interp(doy = ts$doy, x = ts$ts_x, y = ts$ts_y, maxPadLength = 15000), "data.table")
})
