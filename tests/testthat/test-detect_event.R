context("Test detect_event.R")

test_that("detect() returns the correct lists, data.frame, data.table, and columns", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res1 <- detect_event(ts)
  res_event <- res1$event
  expect_is(res1, "list")
  expect_s3_class(res1$climatology, "data.frame")
  expect_s3_class(res1$event, "data.frame")
  expect_false(S3Class(res1$climatology) == "data.table")
  expect_false(S3Class(res1$event) == "data.table")
  expect_equal(ncol(res1$climatology), 9)
  expect_equal(ncol(res1$event), 22)
  res2 <- detect_event(ts, returnDF = FALSE)
  expect_is(res2, "list")
  expect_s3_class(res2$climatology, "data.table")
  expect_s3_class(res2$event, "data.table")
  expect_equal(ncol(res2$climatology), 9)
  expect_equal(ncol(res2$event), 22)
})

test_that("all starting error checks flag correctly", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_error(detect_event(ts, minDuration = "5"),
               "Please ensure that 'minDuration' is a numeric/integer value.")
  expect_error(detect_event(ts, joinAcrossGaps = "TRUE"),
               "Please ensure that 'joinAcrossGaps' is either TRUE or FALSE.")
  expect_error(detect_event(ts, maxGap = "2"),
               "Please ensure that 'maxGap' is a numeric/integer value.")
})

test_that("coldSpells = TRUE returns MCS calculations", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), pctile = 10)
  res <- detect_event(ts, coldSpells = TRUE)
  expect_equal(ncol(res$event), 22)
  expect_lt(min(res$event$intensity_max), 0)
})

test_that("joinAcrossGaps = FALSE returns more events", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_join <- detect_event(ts)
  res_misanthrope <- detect_event(ts, joinAcrossGaps = FALSE)
  expect_lt(nrow(res_join$event), nrow(res_misanthrope$event))
})

test_that("events starting/ending before/after the time series dates are dealt with", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_sub <- ts[885:926, ]
  res <- detect_event(ts_sub)
  res_event <- res$event
  expect_equal(is.na(res_event$rate_onset[1]), TRUE)
  expect_equal(is.na(res_event$rate_decline[3]), TRUE)
})

test_that("detect_event() does not joinAcrossGaps if conditions are not met", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_1 <- detect_event(ts, maxGap = 0)
  res_2 <- detect_event(ts)
  expect_lt(nrow(res_2$event), nrow(res_1$event))
})

test_that("detect_event() utilises the second threshold correctly", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts2 <- exceedance(sst_WA, threshold = 25)
  second_threshold <- ts2$threshold$threshCriterion
  res_1 <- detect_event(ts, threshClim2 = second_threshold)
  res_2 <- detect_event(ts)
  expect_gt(nrow(res_2$event), nrow(res_1$event))
  expect_error(detect_event(ts, threshClim2 = "aaa"))
})

test_that("no detected events returns a 1 row NA event dataframe and not an error", {
  sst_WA_flat <- sst_WA
  sst_WA_flat$temp <- 1
  res <- detect_event(ts2clm(sst_WA_flat, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_is(res, "list")
  expect_is(res$climatology, "data.frame")
  expect_is(res$event, "data.frame")
  expect_equal(ncol(res$climatology), 9)
  expect_equal(ncol(res$event), 22)
  expect_equal(nrow(res$event), 1)
})

test_that("protoEvents argument functions correctly", {
  res <- detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31")), protoEvents = T)
  expect_is(res, "data.frame")
})

test_that("roundRes argument functions correctly", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts, roundRes = 4)
  expect_equal(res$climatology$seas[1], 21.6080)
  res <- detect_event(ts, roundRes = 0)
  expect_equal(res$event$intensity_max[1], 2)
  res <- detect_event(ts, roundRes = F)
  expect_gt(res$event$rate_decline[1], 0.1782399)
  expect_error(detect_event(ts, roundRes = "Banana"),
               "Please ensure that 'roundRes' is either a numeric value or FALSE.")
})

test_that("only one event with NA for rate_onset or rate_decline returns NA and not error", {
  res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_onset <- detect_event(res_clim[885:892,])
  res_decline <- detect_event(res_clim[882:890,])
  res_both <- detect_event(res_clim[885:891,])
  expect_equal(res_onset$event$rate_onset, NA)
  expect_equal(res_decline$event$rate_decline, NA)
  expect_equal(res_both$event$rate_onset, NA)
  expect_equal(res_both$event$rate_decline, NA)
})

test_that("useful error is returned when incorrect column names exist", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  colnames(ts) <- c("doy", "banana", "temp", "seas", "thresh")
  expect_error(detect_event(ts),
               "Please ensure that a column named 't' is present in your data.frame or that you have assigned a column to the 'x' argument.")
  colnames(ts) <- c("doy", "t", "banana", "seas", "thresh")
  expect_error(detect_event(ts),
               "Please ensure that a column named 'temp' is present in your data.frame or that you have assigned a column to the 'y' argument.")
  colnames(ts) <- c("doy", "t", "temp", "banana", "thresh")
  expect_error(detect_event(ts),
               "Please ensure that a column named 'seas' is present in your data.frame or that you have assigned a column to the 'seasClim' argument.")
  colnames(ts) <- c("doy", "t", "temp", "seas", "banana")
  expect_error(detect_event(ts),
               "Please ensure that a column named 'thresh' is present in your data.frame or that you have assigned a column to the 'threshClim' argument.")
})

test_that("lat + latitude columns are passed to category internally", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts$lat <- 10
  res_S <- detect_event(ts, categories = T)
  res_N <- detect_event(ts, categories = T, lat_col = T)
  expect_equal(res_S$season[1], "Fall")
  expect_equal(res_N$season[1], "Spring")
  colnames(ts)[6] <- "latitude"
  res_S <- detect_event(ts, categories = T)
  res_N <- detect_event(ts, categories = T, lat_col = T)
  expect_equal(res_S$season[1], "Fall")
  expect_equal(res_N$season[1], "Spring")
})

test_that("Other built in 'categories' argument works as expected", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts$banana <- "Banana"
  ts_name <- ts
  colnames(ts_name)[3] <- "temperature"
  res_event <- detect_event(ts, categories = TRUE)
  res_list <- detect_event(ts, categories = TRUE, climatology = TRUE)
  res_season <- detect_event(ts, categories = TRUE, season = "peak")
  res_name <- detect_event(ts_name, y = temperature, categories = TRUE, climatology = TRUE)
  res_MCS <- detect_event(ts, coldSpells = TRUE, categories = TRUE, climatology = TRUE,
                          season = "peak", MCScorrect = TRUE, MCSice = TRUE)
  expect_s3_class(res_event, "data.frame")
  expect_is(res_list, "list")
  expect_contains(colnames(res_list$climatology), "banana")
  expect_equal(res_event$category[1], "I Moderate")
  expect_equal(res_list$climatology$category[889], "I Moderate")
  expect_equal(res_season$season[3], "Winter")
  expect_equal(res_name$event$p_moderate[3], 100)
  res_event_DT <- detect_event(ts, categories = TRUE, returnDF = FALSE)
  expect_s3_class(res_event_DT, "data.table")
})

test_that("hourly functions are acknowledged and used", {
  Sys.setenv(TZ = "UTC")
  ts_WA <- sst_WA
  ts_hours <- expand.grid(ts_WA$t, seq(1:24)-1)
  colnames(ts_hours) <- c("t", "hour")
  ts_hours$hourly <- fasttime::fastPOSIXct(paste0(ts_hours$t," ",ts_hours$hour,":00:00"))
  ts_WA_hourly <- merge(ts_hours, ts_WA)
  ts_WA_hourly$temp <- ts_WA_hourly$temp + runif(n = nrow(ts_WA_hourly), min = 0.01, max = 0.1)
  ts_WA_hourly <- ts_WA_hourly[,c("hourly", "temp")]
  colnames(ts_WA_hourly) <- c("t", "temp")
  ts_WA_hourly <- ts_WA_hourly[order(ts_WA_hourly$t),]
  ts_90 <- ts2clm(ts_WA_hourly, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                  windowHalfWidth = 5*24, smoothPercentileWidth = 31*24)
  res_MHW <- detect_event(ts_90, minDuration = 5*24, maxGap = 2*24)
  expect_is(res_MHW$event, "data.frame")
  expect_equal(ncol(res_MHW$event), 22)
  ts_10 <- ts2clm(ts_WA_hourly, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                  windowHalfWidth = 5*24, smoothPercentileWidth = 31*24, pctile = 10)
  res_MCS <- detect_event(ts_10, minDuration = 5*24, maxGap = 2*24, coldSpells = TRUE)
  expect_is(res_MCS$event, "data.frame")
  expect_equal(ncol(res_MCS$event), 22)
})
