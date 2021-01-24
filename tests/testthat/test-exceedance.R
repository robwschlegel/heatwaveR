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

test_that("no exceedances returns a 1 row NA exceedance dataframe and not an error", {
  res_high <- exceedance(data = sst_Med, threshold = 30)
  res_low <- exceedance(data = sst_Med, threshold = 10, below = T)
  expect_is(res_high, "list")
  expect_is(res_low, "list")
  expect_is(res_high$threshold, "tbl_df")
  expect_is(res_low$threshold, "tbl_df")
  expect_is(res_high$exceedance, "tbl_df")
  expect_is(res_low$exceedance, "tbl_df")
  expect_equal(ncol(res_high$threshold), 7)
  expect_equal(ncol(res_low$threshold), 7)
  expect_equal(ncol(res_high$exceedance), 18)
  expect_equal(ncol(res_low$exceedance), 18)
  expect_equal(nrow(res_high$exceedance), 1)
  expect_equal(nrow(res_low$exceedance), 1)
  expect_equal(res_high$exceedance$exceedance_no[1], NA)
  expect_equal(res_low$exceedance$exceedance_no[1], NA)
})

test_that("below argument creates negative values", {
  res <- exceedance(data = sst_Med, threshold = 15, below = TRUE)
  expect_lt(res$exceedance$intensity_max[1], 0)
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
