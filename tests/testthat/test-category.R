context("Test category.R")

test_that("category() returns the correct tibbles and columns", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  cat_res <- category(res)
  expect_is(cat_res, "tbl_df")
  expect_equal(ncol(cat_res), 11)
})

test_that("The seasons by hemisphere come out correctly", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  cat_res_south <- category(res, S = T)
  cat_res_north <- category(res, S = F)
  expect_equal(cat_res_south$season[1], "Fall")
  expect_equal(cat_res_north$season[1], "Spring")
})

test_that("The name argument works correctly", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(data = ts)
  cat_res_banana <- category(res, name = "Banana")
  cat_res_pawpaw <- category(res, name = "Pawpaw")
  expect_equal(droplevels(cat_res_banana$event_name[70]), as.factor("Banana 2014"))
  expect_equal(droplevels(cat_res_pawpaw$event_name[70]), as.factor("Pawpaw 2014"))
})

test_that("y = any existing column", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(data = ts)
  res$climatology$pawpaw <- res$climatology$temp
  expect_is(category(res, y = pawpaw), "tbl_df")
})

test_that("season splits work under all circumstances", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts$temp[1000:1500] <- 24
  ts$temp[2000:2200] <- 22
  ts$temp[4000:4200] <- 22
  res <- detect_event(ts)
  cat_res <- category(res, S = F)
  expect_equal(cat_res$season[70], "Summer-Winter")
  expect_equal(cat_res$season[71], "Year-round")
  expect_equal(cat_res$season[72], "Fall-Spring")
})
