context("Test exceedance.R")

test_that("exceedance() returns the correct lists, tibbles, and columns", {
  res <- exceedance(data = sst_Med, threshold = 20)
  expect_is(res, "list")
  expect_is(res$threshold, "tbl_df")
  expect_is(res$exceedance, "tbl_df")
  expect_equal(ncol(res$threshold), 7)
  expect_equal(ncol(res$exceedance), 18)
})

test_that("threshold may not be missing", {
  expect_error(exceedance(data = sst_Med),
               "Oh no! Please provide a threshold against which to calculate exceedances.")
})

test_that("threshold may not exceed the max temperature in the data", {
  expect_error(exceedance(data = sst_Med, threshold = 30),
               "The given threshold value of 30 is greater than the maximum temperature of 28.78 present in this time series.")
})

test_that("threshold may not exceed the min temperature in the data", {
  expect_error(exceedance(data = sst_Med, threshold = 10),
               "The given threshold value of 10 is less than the minimum temperature of 11.2 present in this time series.")
})

test_that("below argument creates negative values", {
  res <- exceedance(data = sst_Med, threshold = 15, below = TRUE)
  expect_lt(res$exceedance$intensity_max[1], 0)
})

test_that("threshold must be exceeded by enough to be able to detect events", {
  expect_error(exceedance(sst_Med, threshold = 28.7),
               "Not enough consecutive days above 28.7 to detect an event.")
  expect_error(exceedance(sst_Med, threshold = 11.5, below = T),
               "Not enough consecutive days below 11.5 to detect an event.")
})

test_that("joinAcrossGaps = F creates more events", {
  res1 <- exceedance(sst_Med, threshold = 20)
  res2 <- exceedance(sst_Med, threshold = 20, joinAcrossGaps = F)
  expect_lt(nrow(res1$exceedance), nrow(res2$exceedance))
})

test_that("conditionals for calculating exceedance_rel_thresh are responsive", {
  ts <- sst_Med[526:988,]
  res <- exceedance(ts, threshold = 20)
  res_exc <- res$exceedance
  expect_equal(is.na(res_exc$rate_onset[1]), TRUE)
  expect_equal(is.na(res_exc$rate_decline[53]), TRUE)
})

test_that("gaps are not joined if none exist", {
  ts <- sst_Med[150:170, ]
  res <- exceedance(ts, threshold = 20)
  expect_equal(nrow(res$exceedance), 1)
})

test_that("decimal places are rounded to the fourth place", {
  res <- exceedance(data = sst_Med, threshold = 20)
  expect_equal(nchar(strsplit(as.character(res$exceedance$intensity_var[1]), "\\.")[[1]][2]), 4)
})

test_that("maxPadLength argument works correctly throughout", {
  expect_error(exceedance(data = sst_Med, threshold = 20, maxPadLength = "2"),
               "Please ensure that 'maxPadLength' is either FALSE or a numeric/integer value.")
  expect_error(exceedance(data = sst_Med, threshold = 20, maxPadLength = TRUE),
               "Please ensure that 'maxPadLength' is either FALSE or a numeric/integer value.")
  sst_Med_NA <- sst_Med[c(1:20,22:1200),]
  res <- exceedance(data = sst_Med_NA, threshold = 20, maxPadLength = 2)
  expect_equal(round(res$threshold$temp[21], 2), 13.57)
})
