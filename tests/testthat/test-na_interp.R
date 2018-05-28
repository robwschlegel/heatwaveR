context("Test na_interp.R")

test_that("na_interp() doesn't fall over", {
  ts <- heatwaveR:::make_whole_fast(sst_WA)
  res <- heatwaveR:::na_interp(doy = ts$doy, x = ts$t, y = ts$temp)
  expect_is(res, "data.table")
})

test_that("na_interp() handles unusual maxPadLength values correctly", {
  ts <- heatwaveR:::make_whole_fast(sst_WA)
  expect_is(heatwaveR:::na_interp(doy = ts$doy, x = ts$t, y = ts$temp, maxPadLength = 0), "data.table")
  expect_is(heatwaveR:::na_interp(doy = ts$doy, x = ts$t, y = ts$temp, maxPadLength = 15000), "data.table")
})
