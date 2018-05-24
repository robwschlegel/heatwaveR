context("Test detect_event.R")

test_that("detect() returns the correct lists, tibbles, and columns", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  expect_is(res, "list")
  expect_is(res$climatology, "tbl_df")
  expect_is(res$event, "tbl_df")
  expect_equal(ncol(res$climatology), 10)
  expect_equal(ncol(res$event), 22)
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
  expect_equal(nrow(res$event), 71)
  expect_lt(min(res$event$intensity_max), 0)
})

test_that("joinAcrosGaps = FALSE returns more events", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_join <- detect_event(ts)
  res_misanthrope <- detect_event(ts, joinAcrossGaps = FALSE)
  expect_lt(nrow(res_join$event), nrow(res_misanthrope$event))
})

test_that("events starting/ending before/after the time series dates aredealt with", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_sub <- ts[900:11950, ]
  res <- detect_event(ts_sub)
  res_event <- res$event
  expect_equal(is.na(res_event$rate_onset[1]), TRUE)
  expect_equal(is.na(res_event$rate_decline[58]), TRUE)
})

test_that("detect_event() does not joinAcrossGaps if conditions are not met", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_1 <- detect_event(ts, maxGap = 0)
  res_2 <- detect_event(ts)
  expect_lt(nrow(res_2$event), nrow(res_1$event))
})
