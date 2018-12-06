context("Test make_whole_fast.R")

test_that("make_whole_fast() returns a three column data.table", {
  sst_Med_prep <- sst_Med %>%
    dplyr::rename(ts_x = t, ts_y = temp)
  expect_is(make_whole_fast(sst_Med_prep), "data.table")
  expect_equal(ncol(make_whole_fast(sst_Med_prep)), 3)
})
