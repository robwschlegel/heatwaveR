context("Test category.R")

test_that("category() returns the correct tibbles and columns", {
  ts <- make_whole(sst_Med)
  res <- detect(data = ts, climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  cat_res <- category(res)
  expect_is(cat_res, "tbl_df")
  expect_equal(ncol(cat_res), 11)
})

test_that("The seasons by hemisphere come out correctly", {
  ts <- make_whole(sst_Med)
  res <- detect(data = ts, climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  cat_res_south <- category(res, hemisphere = "South")
  cat_res_north <- category(res, hemisphere = "North")
  expect_equal(cat_res_south$season[1], "Fall")
  expect_equal(cat_res_north$season[1], "Spring")
})

test_that("The name argument works correctly", {
  ts <- make_whole(sst_Med)
  res <- detect(data = ts, climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  cat_res_banana <- category(res, name = "Banana")
  cat_res_pawpaw <- category(res, name = "Pawpaw")
  expect_equal(droplevels(cat_res_banana$event_name[1]), as.factor("Banana 1982"))
  expect_equal(droplevels(cat_res_pawpaw$event_name[1]), as.factor("Pawpaw 1982"))
})

test_that("y = any existing column", {
  ts <- make_whole(sst_Med)
  res <- detect(data = ts, climatology_start = "1983-01-01", climatology_end = "2012-12-31")
  res$clim$pawpaw <- res$clim$temp
  expect_is(category(res, y = pawpaw), "tbl_df")
})

## Test that season splits work under broad circumstances
