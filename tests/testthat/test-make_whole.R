context("Test make_whole.R")

test_that("make_whole() returns a three column dataframe", {
  expect_is(heatwaveR:::make_whole(data = sst_Med), "data.frame")
  expect_equal(ncol(heatwaveR:::make_whole(data = sst_Med)), 3)
})

test_that("x = any existing column", {
  sst_Med$pawpaw <- sst_Med$t
  expect_is(heatwaveR:::make_whole(data = sst_Med, x = pawpaw), "data.frame")
  expect_error(heatwaveR:::make_whole(data = sst_Med, x = banana))
})

test_that("y = any existing column", {
  sst_Med$pawpaw <- sst_Med$temp
  expect_is(heatwaveR:::make_whole(data = sst_Med, y = pawpaw), "data.frame")
  expect_error(heatwaveR:::make_whole(data = sst_Med, y = banana))
})

