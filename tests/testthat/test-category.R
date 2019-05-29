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
  res <- detect_event(ts)
  cat_res_banana <- category(res, name = "Banana")
  cat_res_pawpaw <- category(res, name = "Pawpaw")
  expect_equal(droplevels(cat_res_banana$event_name[90]), as.factor("Banana 2014"))
  expect_equal(droplevels(cat_res_pawpaw$event_name[90]), as.factor("Pawpaw 2014"))
})

test_that("y = any existing column", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
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
  expect_equal(cat_res$season[93], "Year-round")
  expect_equal(cat_res$season[35], "Summer/Fall")
  expect_equal(cat_res$season[66], "Winter/Spring")
  expect_equal(cat_res$season[73], "Spring/Summer")
  expect_equal(cat_res$season[88], "Summer-Winter")
  expect_equal(cat_res$season[94], "Fall-Spring")
})

test_that("y = any existing column", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  res$climatology$pawpaw <- res$climatology$temp
  expect_is(category(res, y = pawpaw), "tbl_df")
})

test_that("climatology = T causes a list output with the time series category data", {
  res <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  cat <- category(res, climatology = T)
  expect_is(cat, "list")
  expect_is(cat$climatology, "tbl_df")
  expect_is(cat$event, "tbl_df")
  expect_equal(ncol(cat$climatology), 4)
  expect_equal(ncol(cat$event), 11)
})

test_that("no detected events returns an empty dataframe and not an error", {
  sst_WA_flat <- sst_WA
  sst_WA_flat$temp <- 1
  # sst_WA_flat$temp[4:6] <- 5
  # ts_xy <- ts2clm(sst_WA_flat, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  suppressWarnings(res <- detect_event(ts2clm(sst_WA_flat, climatologyPeriod = c("1983-01-01", "2012-12-31"))))
  cat_event <- category(res, climatology = F)
  cat_clim <- category(res, climatology = T)
  expect_is(cat_event, "tbl_df")
  expect_is(cat_clim, "list")
  expect_is(cat_clim$climatology, "tbl_df")
  expect_equal(nrow(cat_event), 0)
  expect_equal(ncol(cat_event), 11)
  expect_equal(nrow(cat_clim$climatology), 0)
  expect_equal(ncol(cat_clim$climatology), 4)
})
