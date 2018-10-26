context("Test ts2clm.R")

test_that("ts2clm() returns the correct output", {
  res <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 5)
  expect_equal(nrow(res), 12053)
})

test_that("all starting error checks flag correctly", {
  expect_error(ts2clm(sst_WA)) # Specific error not supplied as R sees them as different some how...
  expect_error(ts2clm(sst_WA, climatologyPeriod = "1983-01-01"),
               "Bummer! Please provide BOTH start and end dates for the climatology period.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      robust = "TRUE"),
               "Please ensure that 'robust' is either TRUE or FALSE.")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                      maxPadLength = "2"),
               "Please ensure that 'maxPadLength' is a numeric/integer value.")
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
})

test_that("the start/end dates must not be before/after the clim limits", {
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1973-01-01", "2012-12-31")),
               "The specified start date precedes the first day of series, which is 1982-01-01")
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2023-12-31")),
               "The specified end date follows the last day of series, which is 2014-12-31")
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

test_that("robust = TRUE notifies the user that the 'robust' argument has been deprecated", {
  expect_message(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE),
                 "The 'robust' argument has been deprecated and will be removed from future versions.")
})

test_that("climatologyPeriod less than three years is rejected", {
  expect_error(ts2clm(sst_WA, climatologyPeriod = c("2011-01-01", "2012-12-31")),
               "The climatologyPeriod must be at least three years to calculate thresholds")
})

test_that("mssing data causes na_interp() to be used", {
  sst_WA_NA <- sst_WA
  sst_WA_NA$temp[c(1, 400, 1000)] <- NA
  ts_1 <- ts2clm(sst_WA_NA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_2 <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_condition(ts_1$temp[1], regexp = NA)
  expect_lt(ts_2$temp[400], ts_1$temp[400])
  expect_lt(ts_2$temp[1000], ts_1$temp[1000])
})

test_that("contiguous mssing data causes clim_calc() to be used", {
  sst_WA_cont <- sst_WA %>%
    dplyr::mutate(month = month(t)) %>%
    dplyr::filter(month != 1) %>%
    dplyr::select(-month)
  res <- ts2clm(sst_WA_cont, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 5)
  expect_equal(nrow(res), 12022)
})

test_that("decimal places are rounded to the fourth place", {
  res <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_equal(nchar(strsplit(as.character(res$seas[2]), "\\.")[[1]][2]), 4)
})
