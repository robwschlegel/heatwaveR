context("Test proto_event3.R")

test_that("proto_event3() returns the correct output", {
  ts_xy <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  colnames(ts_xy) <- c("ts_x", "ts_y", "ts_seas", "ts_thresh")
  ts_xy$threshCriterion <- ts_xy$ts_y > ts_xy$ts_thresh
  res <- heatwaveR:::proto_event3(ts_xy, joinAcrossGaps = TRUE,
                                  criterion_column = ts_xy$threshCriterion,
                                  minDuration = 5, maxGap = 2)
  expect_s3_class(res, "data.table")
  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), 14975)
})

test_that("joinAcrossGaps = FALSE creates more events", {
  ts1 <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  colnames(ts1) <- c("ts_x", "ts_y", "ts_seas", "ts_thresh")
  ts1$threshCriterion <- ts1$ts_y > ts1$ts_thresh
  res1 <- heatwaveR:::proto_event3(ts1, joinAcrossGaps = TRUE,
                                   criterion_column = ts1$threshCriterion,
                                   minDuration = 5, maxGap = 2)
  res2 <- heatwaveR:::proto_event3(ts1, joinAcrossGaps = TRUE,
                                   criterion_column = ts1$threshCriterion,
                                   minDuration = 5, maxGap = 5)
  res3 <- heatwaveR:::proto_event3(ts1, joinAcrossGaps = FALSE,
                                   criterion_column = ts1$threshCriterion,
                                   minDuration = 5, maxGap = 2)
  res4 <- heatwaveR:::proto_event3(ts1, joinAcrossGaps = TRUE,
                                   criterion_column = ts1$threshCriterion,
                                   minDuration = 5, maxGap = 5)
  # NB: Replace this with a test once joinAcrossGaps functions as expected
  # This is particularly strange considering that
  # proto_event3 works correctly when called within detect_event3...
  expect_s3_class(res1, "data.table"); expect_s3_class(res2, "data.table")
  expect_s3_class(res3, "data.table"); expect_s3_class(res4, "data.table")
  # expect_gt(max(res2$event_no, na.rm = T), max(res1$event_no, na.rm = T))
})

test_that("no detected events returns columns that all say FALSE/NA and not an error", {
  sst_WA_flat <- sst_WA
  sst_WA_flat$temp <- 1
  ts_xy <- ts2clm3(sst_WA_flat, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  colnames(ts_xy) <- c("ts_x", "ts_y", "ts_seas", "ts_thresh")
  ts_xy$threshCriterion <- ts_xy$ts_y > ts_xy$ts_thresh
  res <- heatwaveR:::proto_event3(ts_xy, joinAcrossGaps = TRUE,
                                  criterion_column = ts_xy$threshCriterion,
                                  minDuration = 5, maxGap = 2)
  expect_s3_class(res, "data.table")
  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), 14975)
  expect_equal(sum(is.na(res$event_no)), 14975)
})

