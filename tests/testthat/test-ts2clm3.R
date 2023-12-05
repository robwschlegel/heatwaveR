context("Test ts2clm3.R")

test_that("ts2clm3() returns the correct output", {
  res <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_s3_class(res, "data.table")
  expect_equal(ncol(res), 4)
  expect_equal(nrow(res), 14975)
})

test_that("all starting error checks flag correctly", {
  expect_error(ts2clm3(sst_WA)) # Specific error not supplied as R sees them as different some how...
  expect_error(ts2clm3(sst_WA, climatologyPeriod = "1983-01-01"),
               "Bummer! Please provide BOTH start and end dates for the climatology period.")
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      maxPadLength = "2"),
               "Please ensure that 'maxPadLength' is either FALSE or a numeric/integer value.")
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      maxPadLength = TRUE),
               "Please ensure that 'maxPadLength' is either FALSE or a numeric/integer value.")
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      pctile = "90"),
               "Please ensure that 'pctile' is a numeric/integer value.")
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      windowHalfWidth = "5"),
               "Please ensure that 'windowHalfWidth' is a numeric/integer value.")
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      smoothPercentile = "FALSE"),
               "Please ensure that 'smoothPercentile' is either TRUE or FALSE.")
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      smoothPercentileWidth = "31"),
               "Please ensure that 'smoothPercentileWidth' is a numeric/integer value.")
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      clmOnly = "FALSE"),
               "Please ensure that 'clmOnly' is either TRUE or FALSE.")
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      roundClm = "2"),
               "Please ensure that 'roundClm' is either a numeric value or FALSE.")
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      roundClm = TRUE),
               "Please ensure that 'roundClm' is either a numeric value or FALSE.")
  sst_WA_dummy1 <- sst_WA
  sst_WA_dummy1$t <- as.POSIXct(sst_WA_dummy1$t)
  expect_error(ts2clm3(sst_WA_dummy1, climatologyPeriod = c("1983-01-01", "2012-12-31")),
               "Please ensure your date values are type 'Date'. This may be done with 'as.Date()")
  sst_WA_dummy2 <- sst_WA
  sst_WA_dummy2$temp <- as.character(sst_WA_dummy2$temp)
  expect_error(ts2clm3(sst_WA_dummy2, climatologyPeriod = c("1983-01-01", "2012-12-31")),
               "Please ensure the temperature values you are providing are type 'num' for numeric.")
})

test_that("the start/end dates must not be before/after the clim limits", {
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1973-01-01", "2012-12-31")),
               "The specified start date precedes the first day of series, which is 1982-01-01")
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2023-12-31")),
               "The specified end date follows the last day of series, which is 2022-12-31")
})

test_that("smooth_percentile = FALSE prevents smoothing", {
  res_smooth <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_chunky <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                       smoothPercentile = FALSE)
  expect_lt(var(res_smooth$seas), var(res_chunky$seas))
})

test_that("clmOnly = TRUE returns only the clim data", {
  res <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), clmOnly = TRUE)
  expect_s3_class(res, "data.table")
  expect_equal(ncol(res), 3)
  expect_equal(nrow(res), 366)
})

test_that("climatologyPeriod less than three years is rejected", {
  expect_error(ts2clm3(sst_WA, climatologyPeriod = c("2011-01-01", "2012-12-31")),
               "The climatologyPeriod must be at least three years to calculate thresholds")
})

test_that("maxPadLength behaves as expected", {
  sst_Med_NA <- sst_Med[c(1:20,22:12000),]
  res1 <- ts2clm3(data = sst_Med_NA, climatologyPeriod = c("1983-01-01", "2012-12-31"), maxPadLength = 2)
  res2 <- ts2clm3(data = sst_Med_NA, climatologyPeriod = c("1983-01-01", "2012-12-31"), maxPadLength = 0)
  res3 <- ts2clm3(data = sst_Med_NA, climatologyPeriod = c("1983-01-01", "2012-12-31"), maxPadLength = -1)
  res4 <- ts2clm3(data = sst_Med_NA, climatologyPeriod = c("1983-01-01", "2012-12-31"), maxPadLength = 15000)
  expect_equal(nrow(res1), 12000); expect_equal(nrow(res2), 12000)
  expect_equal(nrow(res3), 12000); expect_equal(nrow(res4), 12000)
  expect_equal(round(res1$temp[21], 2), 13.57); expect_equal(round(res4$temp[21], 2), 13.57)
  expect_equal(res2$temp[21], as.numeric(NA)); expect_equal(res3$temp[21], as.numeric(NA))
})

test_that("contiguous missing data causes clim_calc() to be used", {
  sst_WA_cont <- sst_WA
  sst_WA_cont$month <- as.numeric(format(sst_WA_cont$t, "%m"))
  sst_WA_cont <- sst_WA_cont[sst_WA_cont$month != 1,]
  sst_WA_cont$month <- NULL
  res <- ts2clm3(sst_WA_cont, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_s3_class(res, "data.table")
  expect_equal(ncol(res), 4)
  expect_equal(nrow(res), 14944)
})

test_that("var argument functions correctly", {
  res <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), var = TRUE)
  expect_s3_class(res, "data.table")
  expect_equal(ncol(res), 5)
  expect_equal(nrow(res), 14975)
})

test_that("roundClm argument functions correctly", {
  res <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), roundClm = 4)
  expect_equal(res$seas[1], 21.6080)
  res <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), roundClm = 0)
  expect_equal(res$seas[1], 22)
  res <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), roundClm = F)
  expect_gt(res$seas[1], 21.60802)
})

test_that("Useful error is returned when incorrect column names exist", {
  ts <- sst_WA
  colnames(ts) <- c("banana", "temp")
  expect_error(ts2clm3(ts, climatologyPeriod = c("1983-01-01", "2012-12-31")),
               "Please ensure that a column named 't' is present in your data.table or that you have assigned a column to the 'x' argument.")
  colnames(ts) <- c("t", "banana")
  expect_error(ts2clm3(ts, climatologyPeriod = c("1983-01-01", "2012-12-31")),
               "Please ensure that a column named 'temp' is present in your data.table or that you have assigned a column to the 'y' argument.")
})

test_that("additional columns in base data should be passed through the funciton", {
  ts_WA <- sst_WA
  ts_WA$site <- "WA"; ts_WA$lon <- 112.625; ts_WA$lat <- -29.375
  ts_WA <- ts_WA[,c(3, 4, 5, 1, 2)]
  res <- ts2clm3(ts_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_s3_class(res, "data.table")
  expect_equal(ncol(res), 7)
  expect_equal(nrow(res), 14975)
})
