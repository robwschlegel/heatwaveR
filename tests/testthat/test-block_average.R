context("Test block_average.R")

test_that("block_average() returns the correct data.frame, data.table, and columns", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  out1 <- block_average(res)
  expect_s3_class(out1, "data.frame")
  expect_false(S3Class(out1) == "data.table")
  expect_equal(ncol(out1), 21)
  out2 <- block_average(res, returnDF = FALSE)
  expect_s3_class(out2, "data.table")
  expect_equal(ncol(out2), 21)
})

test_that("block_average() report argument behaves as expected", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  res_full <- block_average(res, report = "full")
  res_partial <- block_average(res, report = "partial")
  expect_s3_class(res_full, "data.frame")
  expect_s3_class(res_partial, "data.frame")
  expect_lt(nrow(res_partial), nrow(res_full))
  expect_error(block_average(res, report = "part"), "Oops, 'report' must be either 'full' or 'partial'!")
})

test_that("block_average() works with the output of detect_event() and exceedence()", {
  res_de <- detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  res_e <- exceedance(sst_WA, threshold = 25)
  block_de <- block_average(res_de)
  block_e <- block_average(res_e)
  res_false <- exceedance(sst_WA, threshold = 25)$exceedance
  expect_equal(ncol(block_de), 21)
  expect_equal(ncol(block_e), 17)
  expect_error(block_average(res_false))
})
