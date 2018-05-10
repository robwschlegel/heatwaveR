context("Test detect.R")

test_that("detect() returns the correct lists, tibbles, and columns", {
  res <- detect(data = make_whole(sst_Med),
                climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  expect_is(res, "list")
  expect_is(res$clim, "tbl_df")
  expect_is(res$event, "tbl_df")
  expect_equal(ncol(res$clim), 10)
  expect_equal(ncol(res$event), 23)
})

test_that("clim_only argument returns expected results", {
  res <- detect(data = make_whole(sst_Med), clim_only = T,
                climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 6)
})

test_that("join_across_gaps = F creates more events", {
  res1 <- detect(data = make_whole(sst_Med),
                climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  res2 <- detect(data = make_whole(sst_Med), join_across_gaps = FALSE,
                climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  expect_lt(nrow(res1$event), nrow(res2$event))
})

test_that("cold_spells produces negative values", {
  res_cold <- detect(data = make_whole(sst_Med), cold_spells = T,
                 climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  expect_lt(res_cold$event$int_max[1], 0)
})

# test_that("conditionals for calculating mhw_rel_seas_start are responsive", {
#   # This requires that such events may be found and the code tested upon them...
# })

# Not working
# test_that("diff_baseline argument uses a different baseline", {
#   res <- detect(data = make_whole(sst_Med),
#                 diff_baseline = TRUE,
#                 baseline_data = make_whole(sst_Med),
#                 climatology_start = "1983-01-01", climatology_end = "2012-12-31")
#   expect_is(res, "list")
#   expect_is(res$clim, "tbl_df")
#   expect_is(res$event, "tbl_df")
#   expect_equal(ncol(res$clim), 10)
#   expect_equal(ncol(res$event), 23)
# })

# Create test to check that missing climatology_start and climatolog_end do not
# trigger a warning if an alt_clim_data of length 365 or 366 has been provided

# Test that if cold_spells = TRUE & clim_only = TRUE that the output is not negative values

# Test that the alt_clim_data must have the same column names as data
