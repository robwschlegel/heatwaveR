context("Test category.R")

test_that("category() returns the correct data.tables and columns", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  cat_res <- category(res)
  expect_is(cat_res, "data.frame")
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

test_that("latitude columns are detected for seasons instead of S = T/F", {
  ts_Med <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_Med$lat <- 43.625
  ts_WA <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_WA$latitude <- -29.375
  ts_WA_N <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_WA_N$latitude <- 29.375
  ts_NW_Atl_1 <- ts2clm(sst_NW_Atl, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_NW_Atl_1$lati <- 43.125
  ts_NW_Atl_2 <- ts2clm(sst_NW_Atl, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_NW_Atl_2$lat <- 43.125
  cat_Med <- category(detect_event(ts_Med), lat_col = T)
  cat_WA <- category(detect_event(ts_WA), lat_col = T)
  cat_WA_N <- category(detect_event(ts_WA_N), lat_col = T)
  cat_NW_Atl_1 <- category(detect_event(ts_NW_Atl_1), lat_col = T)
  cat_NW_Atl_2 <- category(detect_event(ts_NW_Atl_2), lat_col = T)
  expect_equal(cat_Med$season[1], "Spring")
  expect_equal(cat_WA$season[1], "Fall")
  expect_equal(cat_WA_N$season[1], "Spring")
  expect_equal(cat_NW_Atl_1$season[1], "Summer")
  expect_equal(cat_NW_Atl_2$season[1], "Winter")
})

test_that("latitude column error trapping", {
  ts_1 <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_1$lat <- "43.625"
  ts_2 <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_2$lat <- c(12, rep(5, nrow(ts_2)-1))
  expect_error(category(detect_event(ts_1), lat_col = T))
  expect_error(category(detect_event(ts_2), lat_col = T))
})

test_that("The name argument works correctly", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  cat_res <- category(res)
  cat_res_banana <- category(res, name = "Banana")
  cat_res_pawpaw <- category(res, name = "Pawpaw")
  expect_equal(droplevels(cat_res$event_name[89]), as.factor("Event 2019a"))
  expect_equal(droplevels(cat_res_banana$event_name[90]), as.factor("Banana 2019b"))
  expect_equal(droplevels(cat_res_pawpaw$event_name[110]), as.factor("Pawpaw 1999"))
})

test_that("y = any existing column", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  colnames(res$climatology)[3] <- "pawpaw"
  expect_is(category(res, y = pawpaw), "data.frame")
  expect_error(category(res, y = temp),
               "Please ensure that a column named 'temp' is present in your data.frame or that you have assigned a column to the 'y' argument.")
})

test_that("season splits work under all circumstances", {
  ts <- ts2clm(sst_Med[1:14245,], climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts$temp[1000:1500] <- 24
  ts$temp[2000:2200] <- 22
  ts$temp[4000:4200] <- 22
  res <- detect_event(ts)
  cat_res <- category(res, S = F)
  expect_equal(cat_res$season[110], "Year-round")
  expect_equal(cat_res$season[88], "Summer/Fall")
  expect_equal(cat_res$season[95], "Winter/Spring")
  expect_equal(cat_res$season[97], "Spring/Summer")
  expect_equal(cat_res$season[105], "Summer-Winter")
  expect_equal(cat_res$season[111], "Fall-Spring")
})

test_that("climatology = T causes a list output with the time series category data", {
  res <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  cat <- category(res, climatology = T)
  expect_is(cat, "list")
  expect_is(cat$climatology, "data.frame")
  expect_is(cat$event, "data.frame")
  expect_equal(ncol(cat$climatology), 4)
  expect_equal(ncol(cat$event), 11)
})

test_that("climatology intensity values are correct", {
  res <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  cat_daily <- category(res, climatology = T)$climatology
  expect_is(cat_daily, "data.frame")
  expect_equal(ncol(cat_daily), 4)
  expect_equal(max(cat_daily$intensity), 5.5064)
  expect_equal(min(cat_daily$intensity), 0.3017)
})

test_that("roundVal works as expected", {
  res <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  cat_2 <- category(res, climatology = T, roundVal = 2)
  cat_4 <- category(res, climatology = T, roundVal = 4)
  expect_equal(as.character(max(cat_2$event$i_max)), "5.51")
  expect_equal(as.character(max(cat_4$event$i_max)), "5.5064")
  expect_equal(as.character(min(cat_2$climatology$intensity)), "0.3")
  expect_equal(as.character(min(cat_4$climatology$intensity)), "0.3017")
})

test_that("no detected events returns a 1 row NA dataframe and not an error", {
  sst_WA_flat <- sst_WA
  sst_WA_flat$temp <- 1
  res <- detect_event(ts2clm(sst_WA_flat, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  cat_event <- category(res, climatology = F)
  cat_clim <- category(res, climatology = T)
  expect_is(cat_event, "data.frame")
  expect_is(cat_clim, "list")
  expect_is(cat_clim$climatology, "data.frame")
  expect_equal(nrow(cat_event), 1)
  expect_equal(ncol(cat_event), 11)
  expect_equal(nrow(cat_clim$climatology), 1)
  expect_equal(ncol(cat_clim$climatology), 4)
})

test_that("The presence of only I Moderate events is responded to correctly", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  res_base <- detect_event(ts)
  cat_base <- category(res_base, climatology = T)
  res_Moderate <- res_base
  res_Moderate$climatology <- base::merge(res_Moderate$climatology, cat_base$climatology, by = c("t", "event_no"))
  res_Moderate$climatology$temp[res_Moderate$climatology$category != "I Moderate"] <- as.numeric(NA)
  res_Moderate$climatology <- res_Moderate$climatology[!is.na(res_Moderate$climatology$temp),]
  cat_Moderate <- category(res_Moderate)
  expect_equal(max(cat_Moderate$p_strong), 0)
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
  expect_is(cat_res, "data.frame")
  expect_equal(ncol(cat_res), 11)
  expect_equal(cat_res$i_max[1], -2.6183)
})

test_that("MCS climatology results are correctly inverted to give negative intensities", {
  ts <- ts2clm(sst_Med, climatologyPeriod = c("1983-01-01", "2012-12-31"), pctile = 10)
  res <- detect_event(ts, coldSpells = T)
  cat <- category(res, climatology = T)
  expect_is(cat, "list")
  expect_is(cat$climatology, "data.frame")
  expect_is(cat$event, "data.frame")
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
  expect_equal(as.numeric(table(cat$climatology$category)[1]), 479)
  expect_equal(as.numeric(table(cat_correct$climatology$category)[4]), 271)
})

test_that("MCSice creates a 'V Ice' category only for MCS", {
  sst_ice <- sst_NW_Atl; sst_ice$temp <- sst_ice$temp-5; sst_ice$temp <- ifelse(sst_ice$temp < -1.8, -1.8, sst_ice$temp)
  ts_ice <- ts2clm(sst_ice, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 10)
  res <- detect_event(ts_ice, coldSpells = T)
  cat_ice <- category(res, climatology = T, MCScorrect = T, MCSice = T)
  cat_no_ice <- category(detect_event(ts2clm(sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"))),
                         climatology = T, MCScorrect = T, MCSice = T)
  expect_equal(as.numeric(table(cat_ice$event$category)[5]), 13)
  expect_equal(as.numeric(table(cat_ice$climatology$category)[5]), 89)
  expect_equal(length(table(cat_no_ice$climatology$category)), 4)
})
