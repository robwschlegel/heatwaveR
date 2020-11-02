context("Test category.R")

test_that("category() returns the correct data.tables and columns", {
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
  cat_res <- category(res)
  cat_res_banana <- category(res, name = "Banana")
  cat_res_pawpaw <- category(res, name = "Pawpaw")
  expect_equal(droplevels(cat_res$event_name[89]), as.factor("Event 2012b"))
  expect_equal(droplevels(cat_res_banana$event_name[90]), as.factor("Banana 2014"))
  expect_equal(droplevels(cat_res_pawpaw$event_name[91]), as.factor("Pawpaw 2018a"))
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

test_that("climatology = T causes a list output with the time series category data", {
  res <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  cat <- category(res, climatology = T)
  expect_is(cat, "list")
  expect_is(cat$climatology, "tbl_df")
  expect_is(cat$event, "tbl_df")
  expect_equal(ncol(cat$climatology), 4)
  expect_equal(ncol(cat$event), 11)
})

test_that("climatology intensity values are correct", {
  res <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  cat_daily <- category(res, climatology = T)$climatology
  expect_is(cat_daily, "tbl_df")
  expect_equal(ncol(cat_daily), 4)
  expect_equal(max(cat_daily$intensity), 5.3546)
  expect_equal(min(cat_daily$intensity), 0.519)
})

test_that("roundVal works as expected", {
  res <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  cat_2 <- category(res, climatology = T, roundVal = 2)
  cat_4 <- category(res, climatology = T, roundVal = 4)
  expect_equal(as.character(max(cat_2$event$i_max)), "5.35")
  expect_equal(as.character(max(cat_4$event$i_max)), "5.3546")
  expect_equal(as.character(min(cat_2$climatology$intensity)), "0.52")
  expect_equal(as.character(min(cat_4$climatology$intensity)), "0.519")
})

test_that("no detected events returns an 1 row NA dataframe and not an error", {
  sst_WA_flat <- sst_WA
  sst_WA_flat$temp <- 1
  res <- detect_event(ts2clm(sst_WA_flat, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  cat_event <- category(res, climatology = F)
  cat_clim <- category(res, climatology = T)
  expect_is(cat_event, "tbl_df")
  expect_is(cat_clim, "list")
  expect_is(cat_clim$climatology, "tbl_df")
  expect_equal(nrow(cat_event), 1)
  expect_equal(ncol(cat_event), 11)
  expect_equal(nrow(cat_clim$climatology), 1)
  expect_equal(ncol(cat_clim$climatology), 4)
})

test_that("the different `season` option function as expected", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  expect_equal(category(res, season = "range")$season[9], "Fall/Winter")
  expect_equal(category(res, season = "start")$season[9], "Fall")
  expect_equal(category(res, season = "peak")$season[9], "Fall")
  expect_equal(category(res, season = "end")$season[9], "Winter")
  expect_error(category(res, season = "banana"),
               "Please provide one of the following to the `season` argument: 'range', 'start', 'peak', 'end'.")
})

test_that("category() returns the correct data.tables and columns for MCSs", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"), pctile = 10)
  res <- detect_event(ts, coldSpells = T)
  cat_res <- category(res)
  expect_is(cat_res, "tbl_df")
  expect_equal(ncol(cat_res), 11)
  expect_equal(cat_res$i_max[1], -2.6183)
})

test_that("MCS climatology results are correctly inverted to give negative intensities", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"), pctile = 10)
  res <- detect_event(ts, coldSpells = T)
  cat <- category(res, climatology = T)
  expect_is(cat, "list")
  expect_is(cat$climatology, "tbl_df")
  expect_is(cat$event, "tbl_df")
  expect_equal(ncol(cat$climatology), 4)
  expect_equal(ncol(cat$event), 11)
  expect_equal(cat$climatology$intensity[1], -1.4919)
})

test_that("MCScorrect argument bounds the results to -1.8C", {
  sst_low <- sst_Med; sst_low$temp <- sst_low$temp-20; sst_low$temp <- ifelse(sst_low$temp < -1.8, -1.8, sst_low$temp)
  ts_low <- ts2clm(sst_low, climatologyPeriod = c("1983-01-01", "2012-12-31"), pctile = 10)
  res <- detect_event(ts_low, coldSpells = T)
  cat <- category(res, climatology = T)
  cat_correct <- category(res, climatology = T, MCScorrect = T)
  expect_equal(as.numeric(table(cat$climatology$category)[1]), 475)
  expect_equal(as.numeric(table(cat_correct$climatology$category)[4]), 266)
})
