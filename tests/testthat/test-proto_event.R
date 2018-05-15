context("Test proto_event.R")

test_that("proto_event() returns the correct output", {
  ts_xy <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_xy <- ts_xy[, 2:5]
  colnames(ts_xy) <- c("ts_x", "ts_y", "ts_seas", "ts_thresh")
  ts_xy$ts_y[is.na(ts_xy$ts_y)] <- ts_xy$ts_seas[is.na(ts_xy$ts_y)]
  ts_xy$threshCriterion <- ts_xy$ts_y > ts_xy$ts_thresh
  res <- heatwaveR:::proto_event(ts_xy, criterion_column = 5,
                                 minDuration = 5, maxGap = 2)
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 6)
  expect_equal(nrow(res), 68)
})

test_that("gaps = TRUE returns a different output", {
  ts_xy <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_xy <- ts_xy[, 2:5]
  colnames(ts_xy) <- c("ts_x", "ts_y", "ts_seas", "ts_thresh")
  ts_xy$ts_y[is.na(ts_xy$ts_y)] <- ts_xy$ts_seas[is.na(ts_xy$ts_y)]
  ts_xy$threshCriterion <- ts_xy$ts_y > ts_xy$ts_thresh
  res <- heatwaveR:::proto_event(ts_xy, criterion_column = 5,
                                 minDuration = 5, maxGap = 2, gaps = TRUE)
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 6)
  expect_equal(nrow(res), 44)
})
