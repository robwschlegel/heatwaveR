context("Test ts2clm.R")

test_that("ts2clm() returns the correct output", {
  res1 <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res2 <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), returnDF = FALSE)
  expect_s3_class(res1, "data.frame")
  expect_false(S3Class(res1) == "data.table")
  expect_s3_class(res2, "data.table")
  expect_equal(ncol(res1), 5)
  expect_equal(nrow(res1), 14975)
})

test_that("all starting error checks flag correctly", {
  expect_error(ts2clm(sst_WA)) # Specific error not supplied as R sees them as different some how...
  expect_error(ts2clm(sst_WA, climatologyPeriod = "1983-01-01"),
               "Bummer! Please provide BOTH start and end dates for the climatology period.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      maxPadLength = "2"),
               "Please ensure that 'maxPadLength' is either FALSE or a numeric/integer value.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      maxPadLength = TRUE),
               "Please ensure that 'maxPadLength' is either FALSE or a numeric/integer value.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      pctile = "90"),
               "Please ensure that 'pctile' is a numeric/integer value.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      windowHalfWidth = "5"),
               "Please ensure that 'windowHalfWidth' is a numeric/integer value.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      smoothPercentile = "FALSE"),
               "Please ensure that 'smoothPercentile' is either TRUE or FALSE.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      smoothPercentileWidth = "31"),
               "Please ensure that 'smoothPercentileWidth' is a numeric/integer value.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      clmOnly = "FALSE"),
               "Please ensure that 'clmOnly' is either TRUE or FALSE.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      roundClm = "2"),
               "Please ensure that 'roundClm' is either a numeric value or FALSE.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      roundClm = TRUE),
               "Please ensure that 'roundClm' is either a numeric value or FALSE.")
  sst_WA_dummy1 <- sst_WA
  sst_WA_dummy1$t <- as.character(sst_WA_dummy1$t)
  expect_error(ts2clm(sst_WA_dummy1, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  sst_WA_dummy2 <- sst_WA
  sst_WA_dummy2$temp <- as.character(sst_WA_dummy2$temp)
  expect_error(ts2clm(sst_WA_dummy2, climatologyPeriod = c("1983-01-01", "2012-12-31")),
               "Please ensure the temperature values you are providing are type 'num' for numeric.")
})

test_that("the start/end dates must not be before/after the clim limits", {
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1973-01-01", "2012-12-31")),
               "The specified start date precedes the first day of series, which is 1982-01-01")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2023-12-31")),
               "The specified end date follows the last day of series, which is 2022-12-31")
})

test_that("smooth_percentile = FALSE prevents smoothing", {
  res_smooth <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_chunky <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                       smoothPercentile = FALSE)
  expect_lt(var(res_smooth$seas), var(res_chunky$seas))
})

test_that("clmOnly = TRUE returns only the clim data", {
  res <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                clmOnly = TRUE)
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 3)
  expect_equal(nrow(res), 366)
})

test_that("climatologyPeriod less than three years is rejected", {
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("2011-01-01", "2012-12-31")),
               "The climatologyPeriod must be at least three years to calculate thresholds")
})

test_that("missing data causes na_interp() to be used if a value is provided for maxPadLength", {
  sst_Med_NA <- sst_Med[c(1:20,22:12000),]
  res <- ts2clm(data = sst_Med_NA, climatologyPeriod = c("1983-01-01", "2012-12-31"), maxPadLength = 2)
  expect_equal(nrow(res), 12000)
  expect_equal(round(res$temp[21], 2), 13.57)
})

test_that("contiguous missing data causes clim_calc() to be used", {
  sst_WA_cont <- sst_WA
  sst_WA_cont$month <- as.numeric(format(sst_WA_cont$t, "%m"))
  sst_WA_cont <- sst_WA_cont[sst_WA_cont$month != 1,]
  sst_WA_cont$month <- NULL
  res <- ts2clm(sst_WA_cont, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 5)
  expect_equal(nrow(res), 14944)
})

test_that("decimal places are rounded to the fourth place", {
  res <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_equal(nchar(strsplit(as.character(res$seas[2]), "\\.")[[1]][2]), 4)
})

test_that("var argument functions correctly", {
  res <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), var = T)
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 6)
  expect_equal(nrow(res), 14975)
})

test_that("roundClm argument functions correctly", {
  res <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), roundClm = 4)
  expect_equal(res$seas[1], 21.6080)
  res <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), roundClm = 0)
  expect_equal(res$seas[1], 22)
  res <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), roundClm = F)
  expect_gt(res$seas[1], 21.60802)
})

test_that("Useful error is returned when incorrect column names exist", {
  ts <- sst_WA
  colnames(ts) <- c("banana", "temp")
  expect_error(ts2clm(ts, climatologyPeriod = c("1983-01-01", "2012-12-31")),
               "Please ensure that a column named 't' is present in your data.frame or that you have assigned a column to the 'x' argument.")
  colnames(ts) <- c("t", "banana")
  expect_error(ts2clm(ts, climatologyPeriod = c("1983-01-01", "2012-12-31")),
               "Please ensure that a column named 'temp' is present in your data.frame or that you have assigned a column to the 'y' argument.")
})

test_that("additional columns in base data should be passed through the funciton", {
  ts_WA <- sst_WA
  ts_WA$site <- "WA"; ts_WA$lon <- 112.625; ts_WA$lat <- -29.375
  ts_WA <- ts_WA[,c(3, 4, 5, 1, 2)]
  res <- ts2clm(ts_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), 14975)
})

test_that("hourly functions are acknowledged and used", {
  Sys.setenv(TZ = "UTC")
  ts_WA <- sst_WA[1:3652,]
  ts_hours <- expand.grid(ts_WA$t, seq(1:24)-1)
  colnames(ts_hours) <- c("t", "hour")
  ts_hours$hourly <- fasttime::fastPOSIXct(paste0(ts_hours$t," ",ts_hours$hour,":00:00"))
  ts_WA_hourly <- merge(ts_hours, ts_WA)
  ts_WA_hourly$temp <- ts_WA_hourly$temp + runif(n = nrow(ts_WA_hourly), min = 0.01, max = 0.1)
  ts_WA_hourly <- ts_WA_hourly[,c("hourly", "temp")]
  colnames(ts_WA_hourly) <- c("t", "temp")
  ts_WA_hourly <- ts_WA_hourly[order(ts_WA_hourly$t),]
  res <- ts2clm(ts_WA_hourly, climatologyPeriod = c("1982-01-01", "1991-12-31"),
                windowHalfWidth = 5*24, smoothPercentileWidth = 31*24)
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 6)
  expect_equal(nrow(res), 87648)
  ts_WA_hourly_NA <- ts_WA_hourly; ts_WA_hourly_NA$temp[502] <- NA
  res_var_NA <- ts2clm(ts_WA_hourly_NA, climatologyPeriod = c("1982-01-01", "1991-12-31"), maxPadLength = 2,
                       windowHalfWidth = 5*24, smoothPercentileWidth = 31*24, var = TRUE, returnDF = FALSE)
  expect_is(res_var_NA, "data.table")
  expect_equal(ncol(res_var_NA), 7)
  expect_equal(nrow(res_var_NA), 87648)
  ts_WA_nonhourly <- ts_WA_hourly
  ts_WA_nonhourly$t[1] <- ts_WA_nonhourly$t[1]+61
  expect_error(ts2clm(ts_WA_nonhourly, climatologyPeriod = c("1982-01-01", "1991-12-31")))
})
