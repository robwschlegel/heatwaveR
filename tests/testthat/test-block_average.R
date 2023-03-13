context("Test block_average.R")

test_that("block_average() returns the correct tibble and columns", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  out <- block_average(res)
  expect_is(out, "data.frame")
  expect_equal(ncol(out), 21)
})

test_that("block_average() report argument behaves as expected", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  res_full <- block_average(res, report = "full")
  res_partial <- block_average(res, report = "partial")
  expect_is(res_full, "data.frame")
  expect_is(res_partial, "data.frame")
  expect_lt(nrow(res_partial), nrow(res_full))
  expect_error(block_average(res, report = "part"), "Oops, 'report' must be either 'full' or 'partial'!")
})

test_that("block_average() works with the output of detect_event() and exceedence()", {
  res_de <- detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  res_e <- exceedance(sst_WA, threshold = 25)
  block_de <- block_average(res_de)
  block_e <- block_average(res_e)
  res_false <- exceedance(sst_WA, threshold = 25)$exceedance
  expect_is(block_de, "data.frame")
  expect_is(block_e, "data.frame")
  expect_error(block_average(res_false))
})
