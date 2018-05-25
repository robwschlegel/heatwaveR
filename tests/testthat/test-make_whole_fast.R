context("Test make_whole_fast.R")

test_that("make_whole_fast() returns a three column data.table", {
  expect_is(make_whole_fast(sst_Med), "data.table")
  expect_equal(ncol(make_whole_fast(data = sst_Med)), 3)
})

test_that("x = any existing column", {
  sst_Med$pawpaw <- sst_Med$t
  expect_is(make_whole_fast(data = sst_Med, x = pawpaw), "data.table")
  expect_error(make_whole_fast(data = sst_Med, x = banana))
})

test_that("y = any existing column", {
  sst_Med$pawpaw <- sst_Med$temp
  expect_is(make_whole_fast(data = sst_Med, y = pawpaw), "data.table")
  expect_error(make_whole_fast(data = sst_Med, y = banana))
})
