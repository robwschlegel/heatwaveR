context("Test detect_event3.R")

test_that("detect_event3() returns the correct lists, data.frame, data.table, and columns", {
  ts <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event3(ts)
  res_event <- res$event
  expect_is(res, "list")
  expect_s3_class(res$climatology, "data.table")
  expect_s3_class(res$event, "data.table")
  expect_equal(ncol(res$climatology), 8)
  expect_equal(ncol(res$event), 22)
})

test_that("all starting error checks flag correctly", {
  ts <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_error(detect_event3(ts, minDuration = "5"),
               "'minDuration' must be numeric.")
  expect_error(detect_event3(ts, joinAcrossGaps = "TRUE"),
               "'joinAcrossGaps' must be a boolean value.")
  expect_error(detect_event3(ts, maxGap = "2"),
               "'maxGap' must be numeric.")
})

test_that("coldSpells = TRUE returns MCS calculations", {
  ts <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), pctile = 10)
  res <- detect_event3(ts, coldSpells = TRUE)
  expect_equal(ncol(res$event), 22)
  expect_lt(min(res$event$intensity_max), 0)
})

test_that("joinAcrossGaps = FALSE returns more events", {
  ts <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_join <- detect_event3(ts)
  res_misanthrope <- detect_event3(ts, joinAcrossGaps = FALSE)
  expect_lt(nrow(res_join$event), nrow(res_misanthrope$event))
})

test_that("events starting/ending before/after the time series dates are dealt with", {
  ts <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_sub <- ts[885:926, ]
  res <- detect_event3(ts_sub)
  res_event <- res$event
  expect_equal(is.na(res_event$rate_onset[1]), TRUE)
  expect_equal(is.na(res_event$rate_decline[3]), TRUE)
})

test_that("detect_event3() does not joinAcrossGaps if conditions are not met", {
  ts <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_1 <- detect_event3(ts, maxGap = 0)
  res_2 <- detect_event3(ts)
  expect_lt(nrow(res_2$event), nrow(res_1$event))
})

test_that("detect_event3() utilises the second threshold correctly", {
  ts <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts2 <- exceedance(sst_WA, threshold = 25)
  second_threshold <- ts2$threshold$threshCriterion
  res_1 <- detect_event3(ts, threshClim2 = second_threshold)
  res_2 <- detect_event3(ts)
  expect_gt(nrow(res_2$event), nrow(res_1$event))
  expect_error(detect_event3(ts, threshClim2 = "aaa"))
})

test_that("no detected events returns a 1 row NA event dataframe and not an error", {
  sst_WA_flat <- sst_WA
  sst_WA_flat$temp <- 1
  res <- detect_event3(ts2clm3(sst_WA_flat, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_is(res, "list")
  expect_is(res$climatology, "data.frame")
  expect_is(res$event, "data.frame")
  expect_equal(ncol(res$climatology), 8)
  expect_equal(ncol(res$event), 22)
  expect_equal(nrow(res$event), 1)
})

test_that("protoEvents argument functions correctly", {
  res <- detect_event3(ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31")), protoEvents = T)
  expect_s3_class(res, "data.table")
  expect_equal(ncol(res), 8)
})

test_that("roundRes argument functions correctly", {
  ts <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event3(ts, roundRes = 4)
  expect_equal(res$climatology$ts_seas[1], 21.6080)
  res <- detect_event3(ts, roundRes = 0)
  expect_equal(res$event$intensity_max[1], 2)
  res <- detect_event3(ts, roundRes = F)
  expect_gt(res$event$rate_decline[1], 0.1782399)
  expect_error(detect_event3(ts, roundRes = "Banana"),
               "'roundRes' must be numeric or FALSE.")
})

test_that("only one event with NA for rate_onset or rate_decline returns NA and not error", {
  res_clim <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_onset <- detect_event3(res_clim[885:892,])
  res_decline <- detect_event3(res_clim[882:890,])
  res_both <- detect_event3(res_clim[885:891,])
  expect_equal(res_onset$event$rate_onset, as.numeric(NA))
  expect_equal(res_decline$event$rate_decline, as.numeric(NA))
  expect_equal(res_both$event$rate_onset, as.numeric(NA))
  expect_equal(res_both$event$rate_decline, as.numeric(NA))
})

test_that("useful error is returned when incorrect columns, types, or names exist", {
  ts <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_short <- ts[, 2:4]
  ts_type <- ts; ts_type$temp <- as.character(ts_type$temp)
  ts_name1 <- ts; colnames(ts_name1) <- c("banana", "temp", "seas", "thresh")
  ts_name2 <- ts; colnames(ts_name2) <- c("t", "mango", "seas", "thresh")

  expect_error(detect_event3(ts_short), "Data must contain columns: t, temp, seas, thresh.")
  expect_error(detect_event3(ts_type), "The first four columns must be of types: Date, numeric, numeric, numeric")

  # NB: detect_event3 does not currently check if the columns with changed names have been assigned correctly
  # This isn't necessarily something we have to ensure, It can be up to the user.
  # Note however that this behaviour is implemented in ts2clm3.
  # expect_error(detect_event3(ts),
  #              "Please ensure that a column named 't' is present in your data.frame or that you have assigned a column to the 'x' argument.")
  # expect_error(detect_event3(ts),
  #              "Please ensure that a column named 'temp' is present in your data.frame or that you have assigned a column to the 'y' argument.")
  # colnames(ts) <- c("doy", "t", "temp", "banana", "thresh")
  # expect_error(detect_event3(ts),
  #              "Please ensure that a column named 'seas' is present in your data.frame or that you have assigned a column to the 'seasClim' argument.")
  # colnames(ts) <- c("doy", "t", "temp", "seas", "banana")
  # expect_error(detect_event3(ts),
  #              "Please ensure that a column named 'thresh' is present in your data.frame or that you have assigned a column to the 'threshClim' argument.")

  # But it does correctly recognise name changes
  expect_s3_class(detect_event3(ts_name1, x = banana)$event, "data.table")
  expect_s3_class(detect_event3(ts_name2, y = mango)$event, "data.table")
})

test_that("lat + latitude columns are passed to category internally", {
  ts <- ts2clm3(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts$lat <- 10
  res_S <- detect_event3(ts, categories = T)
  res_N <- detect_event3(ts, categories = T, lat_col = T)
  expect_equal(res_S$season[1], "Fall")
  expect_equal(res_N$season[1], "Spring")
  colnames(ts)[5] <- "latitude"
  res_S <- detect_event3(ts, categories = T)
  res_N <- detect_event3(ts, categories = T, lat_col = T)
  expect_equal(res_S$season[1], "Fall")
  expect_equal(res_N$season[1], "Spring")
})

test_that("Other built in 'categories' argument works as expected", {
  ts <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts$banana <- "Banana"
  ts_name <- ts
  colnames(ts_name)[2] <- "temperature"
  res_event <- detect_event3(ts, categories = TRUE)

  # NB: This throws an error
  # res_list <- detect_event3(ts, categories = TRUE, climatology = TRUE)
  # res_name <- detect_event3(ts_name, y = temperature, categories = TRUE, climatology = TRUE)
  # res_MCS <- detect_event3(ts, coldSpells = TRUE, categories = TRUE, climatology = TRUE,
  #                         season = "peak", MCScorrect = TRUE, MCSice = TRUE)
  res_season <- detect_event3(ts, categories = TRUE, season = "peak")
  expect_s3_class(res_event, "data.frame")
  # expect_is(res_list, "list")
  # expect_contains(colnames(res_list$climatology), "banana")
  expect_equal(res_event$category[1], "I Moderate")
  # expect_equal(res_list$climatology$category[889], "I Moderate")
  expect_equal(res_season$season[3], "Winter")
  # expect_equal(res_name$event$p_moderate[3], 100)
})
