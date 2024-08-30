context("Test graph_functions.R")

# To check specific parts of the plot
# https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
# library(proto)


# event_line tests --------------------------------------------------------

test_that("event_line() doesn't fall over", {
  res <- detect_event(data = ts2clm(sst_Med,
                climatologyPeriod = c("1983-01-01", "2012-12-31")))
  tp <- event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31")
  expect_is(tp, "ggplot")
})

test_that("event_line() arguments can be names different than 'x' or 'temp'", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  res$climatology$banana <- res$climatology$temp
  tp <- event_line(data = res, y = banana, start_date = "2012-01-01", end_date = "2012-12-31")
  expect_is(tp, "ggplot")
})

test_that("event_line() does not require start and end dates to be provided", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_is(event_line(data = res), "ggplot")
  expect_is(event_line(data = res, start_date = "2010-01-01"), "ggplot")
})

test_that("event_line() metric changes accordingly", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  tp_1 <- event_line(data = res, metric = intensity_max)
  tp_2 <- event_line(data = res, metric = duration)
  expect_is(tp_1, "ggplot")
  expect_is(tp_2, "ggplot")
})

test_that("data fed to event_line() is a list with correct dataframes", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_error(event_line(data = res$event, start_date = "2012-01-01", end_date = "2012-12-31"))
})

test_that("data fed to event_line() is a list with correct dataframes", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_error(event_line(data = res, start_date = "2012-05-01", end_date = "2012-07-31"),
               "No events detected! Consider changing the 'start_date' or 'end_date' values.")
})

test_that("event_line() metric must be spelled correctly", {
  res <- detect_event(data = ts2clm(sst_Med,
                climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_error(event_line(data = res, metric = bananana,
                          start_date = "2012-01-01", end_date = "2012-12-31"))
})

test_that("event_line() may create MCS output", {
  res <- detect_event(data = ts2clm(sst_Med, pctile = 10,
                climatologyPeriod = c("1983-01-01", "2012-12-31")), coldSpells = TRUE)
  tp <- event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31")
  expect_is(tp, "ggplot")
})

test_that("event_line() category argument works", {
  res <- detect_event(data = ts2clm(sst_Med,
                climatologyPeriod = c("1983-01-01", "2012-12-31")))
  tp <- event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31",
                   category = TRUE)
  expect_is(tp, "ggplot")
})

test_that("event_line() category argument works for MCSs", {
  res <- detect_event(ts2clm(sst_Med, pctile = 10, climatologyPeriod = c("1983-01-01", "2012-12-31")),
                      coldSpells = TRUE)
  tp <- event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31", category = TRUE)
  expect_is(tp, "ggplot")
})

test_that("event_line() line_colours argument works", {
  res <- detect_event(data = ts2clm(sst_Med, pctile = 90,
                                    climatologyPeriod = c("1982-01-01", "2011-12-31")))
  expect_is(event_line(data = res, line_colours = c(colour = "purple", "pink")), "ggplot")
  expect_is(event_line(data = res, category = T, line_colours = c(colour = "purple", "pink")), "ggplot")
})

test_that("event_line() additional options error traping works", {
  res <- detect_event(data = ts2clm(sst_Med, pctile = 90,
                                    climatologyPeriod = c("1982-01-01", "2011-12-31")))
  expect_error(event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31", x_axis_title = 12),
               "Please ensure that the argument provided to 'x_axis_title' is a character string.")
  expect_error(event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31", y_axis_title = FALSE),
               "Please ensure that the argument provided to 'y_axis_title' is a character string.")
  expect_error(event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31", x_axis_text_angle = "100"),
               "Please ensure that the argument provided to 'x_axis_text_angle' is a number.")
  expect_error(event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31", y_axis_range = 100))
  expect_error(event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31", y_axis_range = c(2, "A")),
               "Please ensure that only numeric values are provided to 'y_axis_range'.")
  expect_error(event_line(data = res, line_colours = data.frame(colour = "black", "blue")))
  expect_error(event_line(data = res, category = TRUE, line_colours = data.frame(colour = "black", "blue")))
})

test_that("event_line() additional plotting work", {
  res <- detect_event(data = ts2clm(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31")))
  p1 <- event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31", x_axis_title = "Date")
  expect_equal(p1$labels$x, "Date")
  p2 <- event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31", y_axis_title = "Temp.")
  expect_equal(p2$labels$y, "Temp.")
  p3 <- event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31", x_axis_text_angle = 10)
  expect_equal(p3$theme$axis.text.x$angle, 10)
  p4 <- event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31", y_axis_range = c(20, 30))
  expect_equal(p4$coordinates$limits$y, c(20, 30))
})

test_that("event_line() responds correctly to hourly data", {
  Sys.setenv(TZ = "UTC")
  ts_WA <- sst_WA[1:3652,]
  ts_hours <- expand.grid(ts_WA$t, seq(1:24)-1)
  colnames(ts_hours) <- c("t", "hour")
  ts_hours$hourly <- fasttime::fastPOSIXct(paste0(ts_hours$t," ",ts_hours$hour,":00:00"))
  ts_WA_hourly <- merge(ts_hours, ts_WA)
  ts_WA_hourly$temp <- ts_WA_hourly$temp + runif(n = nrow(ts_WA_hourly), min = 0.01, max = 0.1)
  ts_WA_hourly <- ts_WA_hourly[,c("hourly", "temp")]
  colnames(ts_WA_hourly) <- c("t", "temp")
  ts_WA_hourly <- ts_WA_hourly[order(ts_WA_hourly$t),]
  ts_res <- ts2clm(ts_WA_hourly, climatologyPeriod = c("1982-01-01", "1991-12-31"),
                windowHalfWidth = 5*24, smoothPercentileWidth = 31*24)
  event_res <- detect_event(data = ts_res)
  expect_error(event_line(data = event_res))
})


# lolli_plot tests --------------------------------------------------------

test_that("lolli_plot() doesn't fall over", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  tp <- lolli_plot(res)
  expect_is(tp, "ggplot")
})

test_that("lolli_plot() argument names can be changed", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  res$event$`Duration [days]` <- res$event$duration
  res$event$`Mean intensity [°C]` <- res$event$intensity_mean
  tp <- lolli_plot(res, xaxis = `Duration [days]`, metric = `Mean intensity [°C]`)
  expect_is(tp, "ggplot")
})

test_that("data fed to lolli_plot() is a list with correct dataframes", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_error(lolli_plot(res$event))
})

test_that("lolli_plot() may not be asked to highlight more events than there are", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_error(lolli_plot(res, event_count = 666),
               "Please ensure that event_count is less or equal to than the total number of events in your results.")
})

test_that("lolli_plot() may create MCS output", {
  res <- detect_event(data = ts2clm(sst_Med, pctile = 10,
                climatologyPeriod = c("1983-01-01", "2012-12-31")), coldSpells = TRUE)
  tp <- lolli_plot(res)
  expect_is(tp, "ggplot")
})

test_that("lolli_plot() correctly highlights no events when told not to", {
  res <- detect_event(data = ts2clm(sst_Med, pctile = 10,
                climatologyPeriod = c("1983-01-01", "2012-12-31")), coldSpells = TRUE)
  tp <- lolli_plot(res, event_count = 0)
  expect_is(tp, "ggplot")
})
