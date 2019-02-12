context("Test proto_event.R")

test_that("proto_event() returns the correct output", {
  ts_xy <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_xy <- ts_xy[, 2:5]
  colnames(ts_xy) <- c("ts_x", "ts_y", "ts_seas", "ts_thresh")
  ts_xy$threshCriterion <- ts_xy$ts_y > ts_xy$ts_thresh
  res <- heatwaveR:::proto_event(ts_xy, joinAcrossGaps = TRUE,
                                 criterion_column = ts_xy$threshCriterion,
                                 minDuration = 5, maxGap = 2)
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), 13514)
})

test_that("joinAcrossGaps = FALSE creates more events", {
  ts_xy <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_xy <- ts_xy[, 2:5]
  colnames(ts_xy) <- c("ts_x", "ts_y", "ts_seas", "ts_thresh")
  ts_xy$threshCriterion <- ts_xy$ts_y > ts_xy$ts_thresh
  res1 <- heatwaveR:::proto_event(ts_xy, joinAcrossGaps = TRUE,
                                  criterion_column = ts_xy$threshCriterion,
                                  minDuration = 5, maxGap = 2)
  res2 <- heatwaveR:::proto_event(ts_xy, joinAcrossGaps = FALSE,
                                  criterion_column = ts_xy$threshCriterion,
                                  minDuration = 5, maxGap = 2)
  expect_gt(max(res2$event_no, na.rm = T), max(res1$event_no, na.rm = T))
})

test_that("no detected events returns an empty dataframe and not an error", {
  sst_WA_flat <- sst_WA
  sst_WA_flat$temp <- 1
  # sst_WA_flat$temp[4:6] <- 5
  ts_xy <- ts2clm(sst_WA_flat, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_xy <- ts_xy[, 2:5]
  colnames(ts_xy) <- c("ts_x", "ts_y", "ts_seas", "ts_thresh")
  ts_xy$threshCriterion <- ts_xy$ts_y > ts_xy$ts_thresh
  res <- heatwaveR:::proto_event(ts_xy, joinAcrossGaps = TRUE,
                                 criterion_column = ts_xy$threshCriterion,
                                 minDuration = 5, maxGap = 2)
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), 13514)
  expect_equal(sum(is.na(res$event_no)), 13514)
})
