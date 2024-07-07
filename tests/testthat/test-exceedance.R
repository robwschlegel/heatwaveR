context("Test exceedance.R")

test_that("exceedance() returns the correct lists, data.frames, data.tables, and columns", {
  res1 <- exceedance(data = sst_Med, threshold = 20)
  expect_is(res1, "list")
  expect_s3_class(res1$threshold, "data.frame")
  expect_s3_class(res1$exceedance, "data.frame")
  expect_false(S3Class(res1$threshold) == "data.table")
  expect_false(S3Class(res1$exceedance) == "data.table")
  expect_equal(ncol(res1$threshold), 7)
  expect_equal(ncol(res1$exceedance), 18)
  res2 <- exceedance(data = sst_Med, threshold = 20, returnDF = FALSE)
  expect_is(res2, "list")
  expect_s3_class(res2$threshold, "data.table")
  expect_s3_class(res2$exceedance, "data.table")
  expect_equal(ncol(res1$threshold), 7)
  expect_equal(ncol(res1$exceedance), 18)
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
  expect_is(res_high$threshold, "data.frame")
  expect_is(res_low$threshold, "data.frame")
  expect_is(res_high$exceedance, "data.frame")
  expect_is(res_low$exceedance, "data.frame")
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
  sst_Med_miss <- sst_Med[c(1:20,22:1200),]
  res <- exceedance(data = sst_Med_miss, threshold = 20, maxPadLength = 2)
  expect_equal(round(res$threshold$temp[21], 2), 13.57)
})

test_that("Useful error is returned when incorrect column names exist", {
  ts <- sst_WA
  colnames(ts) <- c("banana", "temp")
  expect_error(exceedance(ts, threshold = 20),
               "Please ensure that a column named 't' is present in your data.frame or that you have assigned a column to the 'x' argument.")
  colnames(ts) <- c("t", "banana")
  expect_error(exceedance(ts, threshold = 20),
               "Please ensure that a column named 'temp' is present in your data.frame or that you have assigned a column to the 'y' argument.")
})

test_that("Extra columns are passed forward correctly", {
  ts <- sst_WA
  ts$banana <- 1
  ts$mango <- 2
  ts <- ts[, c(3, 4, 1, 2)]
  res1 <- exceedance(data = ts, threshold = 20, maxPadLength = 2)
  res1_thresh <- res1$threshold
  ts_miss1 <- ts[c(1:20, 22:1200),]
  res2 <- exceedance(data = ts_miss1, threshold = 20, maxPadLength = 2)
  res2_thresh <- res2$threshold
  ts_miss2 <- ts_miss1[, c(1, 3, 2, 4)]
  res3 <- exceedance(data = ts_miss2, threshold = 20, maxPadLength = 2)
  res3_thresh <- res3$threshold
  ts_miss3 <- ts; ts_miss3$temp[21] <- NA
  res4 <- exceedance(data = ts_miss3, threshold = 20, maxPadLength = 2)
  res4_thresh <- res4$threshold
  expect_equal(ncol(res1$threshold), 9)
  expect_is(res2$threshold, "data.frame")
  expect_is(res3$threshold, "data.frame")
  expect_equal(nrow(res2$threshold), 1200)
  expect_true(is.na(res3$threshold[21,3]))
  expect_equal(res4$threshold[21,1], 1)
})

test_that("hourly functions are acknowledged and used", {
  Sys.setenv(TZ = "UTC")
  ts_Med <- sst_Med[1:3652,]
  ts_hours <- expand.grid(ts_Med$t, seq(1:24)-1)
  colnames(ts_hours) <- c("t", "hour")
  ts_hours$hourly <- fasttime::fastPOSIXct(paste0(ts_hours$t," ",ts_hours$hour,":00:00"))
  ts_Med_hourly <- merge(ts_hours, ts_Med)
  ts_Med_hourly$temp <- ts_Med_hourly$temp + runif(n = nrow(ts_Med_hourly), min = 0.01, max = 0.1)
  ts_Med_hourly <- ts_Med_hourly[,c("hourly", "temp")]
  colnames(ts_Med_hourly) <- c("t", "temp")
  ts_Med_hourly <- ts_Med_hourly[order(ts_Med_hourly$t),]
  res <- exceedance(data = ts_Med_hourly, threshold = 20, minDuration = 5*24, maxGap = 2*24)
  expect_is(res$exceedance, "data.frame")
  expect_equal(ncol(res$exceedance), 18)
  expect_equal(nrow(res$exceedance), 15)
  ts_Med_nonhourly <- ts_Med_hourly
  ts_Med_nonhourly$t[1] <- ts_Med_nonhourly$t[1]+61
  expect_error(exceedance(ts_Med_nonhourly, threshold = 20))
})
