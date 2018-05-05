context("Test exceedance.R")

test_that("exceedance() returns the correct lists, tibbles, and columns", {
  res <- exceedance(data = make_whole(sst_Med), threshold = 20)
  expect_is(res, "list")
  expect_is(res$threshold, "tbl_df")
  expect_is(res$exceedance, "tbl_df")
  expect_equal(ncol(res$threshold), 7)
  expect_equal(ncol(res$exceedance), 17)
})

test_that("threshold may not be missing", {
  expect_error(exceedance(data = make_whole(sst_Med)),
               "Oh no! Please provide a threshold against which to calculate exceedances.")
})

test_that("threshold may not exceed the max temperature in the data", {
  expect_error(exceedance(data = make_whole(sst_Med), threshold = 30),
               "The given threshold value of 30 is greater than the maximum temperature of 28.15 present in this time series.")
})

test_that("threshold may not exceed the min temperature in the data", {
  expect_error(exceedance(data = make_whole(sst_Med), threshold = 10),
               "The given threshold value of 10 is less than the minimum temperature of 11.2 present in this time series.")
})

test_that("below argument creates negative values", {
  res <- exceedance(data = make_whole(sst_Med), threshold = 15, below = TRUE)
  expect_lt(res$exceedance$int_max[1], 0)
})

test_that("threshold must be exceeded by enough to be able to detect events", {
  ts <- make_whole(sst_Med)
  expect_error(exceedance(data = make_whole(sst_Med), threshold = 28),
               "Not enough consecutive days above 28 to detect an event.")
  expect_error(exceedance(data = make_whole(sst_Med), threshold = 12, below = T),
               "Not enough consecutive days below 12 to detect an event.")
})

test_that("join_across_gaps = F creates more events", {
  res1 <- exceedance(data = make_whole(sst_Med), threshold = 20)
  res2 <- exceedance(data = make_whole(sst_Med), threshold = 20, join_across_gaps = F)
  expect_lt(nrow(res1$exceedance), nrow(res2$exceedance))
})

# test_that("conditionals for calculating exceedance_rel_thresh are responsive", {
#   # This requires that such events may be found and the code tested upon them...
# })
