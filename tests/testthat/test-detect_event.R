context("Test detect_event.R")

test_that("detect() returns the correct lists, tibbles, and columns", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  expect_is(res, "list")
  expect_is(res$clim, "tbl_df")
  expect_is(res$event, "tbl_df")
  expect_equal(ncol(res$clim), 10)
  expect_equal(ncol(res$event), 23)
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

# test_that("the default output of detect_event() matches detect()", {
#   res_old <- detect(data = make_whole(sst_WA),
#                     climatology_start = "1983-01-01", climatology_end = "2012-12-31")
#   res_new <- detect_event(ts2clm(sst_WA,
#                                  climatologyPeriod = c("1983-01-01", "2012-12-31")))
#   expect_equal(sum(res_old$clim$seas_clim_year), sum(res_new$climatology$seas))
#   expect_equal(sum(res_old$clim$thresh_clim_year), sum(res_new$climatology$thresh))
#   expect_equal(sum(res_old$event$int_mean), sum(res_new$event$intensity_mean))
#   expect_equal(sum(res_old$event$int_mean_rel_thresh), sum(res_new$event$intensity_mean_relThresh))
#   expect_equal(sum(res_old$event$int_cum), sum(res_new$event$intensity_cumulative))
# })
#
# test_that("the default cold-spell output of detect_event() matches detect()", {
#   res_old <- detect(data = make_whole(sst_WA), cold_spells = TRUE,
#                     climatology_start = "1983-01-01", climatology_end = "2012-12-31")
#   res_new <- detect_event(ts2clm(sst_WA, pctile = 10,
#                                  climatologyPeriod = c("1983-01-01", "2012-12-31")),
#                           coldSpells = TRUE)
#   expect_equal(sum(res_old$clim$seas_clim_year), sum(res_new$climatology$seas))
#   expect_equal(sum(res_old$clim$thresh_clim_year), sum(res_new$climatology$thresh))
#   expect_equal(sum(res_old$event$int_mean), sum(res_new$event$intensity_mean))
#   expect_equal(sum(res_old$event$int_mean_rel_thresh), sum(res_new$event$intensity_mean_relThresh))
#   expect_equal(sum(res_old$event$int_cum), sum(res_new$event$intensity_cumulative))
# })
