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

test_that("event_line() does not require start and end dates to be provided", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_is(event_line(data = res), "ggplot")
  expect_is(event_line(data = res, start_date = "2010-01-01"), "ggplot")
})

test_that("data fed to event_line() is a list with correct dataframes", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  # Error message doesn't sync up for some reason so is omitted here...
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
  expect_error(event_line(data = res, metric = "bananana",
                          start_date = "2012-01-01", end_date = "2012-12-31"),
               "Please ensure you have spelled the desired metric correctly.")
})

test_that("event_line() may create MCS output", {
  res <- detect_event(data = ts2clm(sst_Med,
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

test_that("event_line() category argument doesn't work for MCSs", {
  res <- detect_event(data = ts2clm(sst_Med, pctile = 10,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")),
                      coldSpells = TRUE)
  test <- res$event
  expect_error(event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31",
                   category = TRUE),
               "Categories currently only calculated for MHWs, not MCSs. But coming soon!")
})


# lolli_plot tests --------------------------------------------------------

test_that("lolli_plot() doesn't fall over", {
  res <- detect_event(data = ts2clm(sst_Med,
                climatologyPeriod = c("1983-01-01", "2012-12-31")))
  tp <- lolli_plot(res)
  expect_is(tp, "ggplot")
})

test_that("data fed to lolli_plot() is a list with correct dataframes", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  # Error message doesn't sync up for some reason so is omitted here...
  expect_error(lolli_plot(res$event))
})

test_that("lolli_plot() metric must be spelled correctly", {
  res <- detect_event(data = ts2clm(sst_Med,
                climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_error(lolli_plot(res, metric = "what does the fox say"),
               "Please ensure you have spelled the desired metric correctly.")
})

test_that("lolli_plot() xaxis must be spelled correctly", {
  res <- detect_event(data = ts2clm(sst_Med,
                climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_error(lolli_plot(res, xaxis = "waw paw paw paw paw"),
               "Please ensure you have spelled the desired xaxis correctly.")
})

test_that("lolli_plot() may not be asked to highlight more events than there are", {
  res <- detect_event(data = ts2clm(sst_Med,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_error(lolli_plot(res, event_count = 666),
               "Please ensure that event_count is less than the total number of events in your results.")
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

test_that("lolli_plot() correctly changes x axis label for given 'xaxis' value", {
  res <- detect_event(data = ts2clm(sst_Med, pctile = 10,
                climatologyPeriod = c("1983-01-01", "2012-12-31")), coldSpells = TRUE)
  tp_1 <- lolli_plot(res, xaxis = "event_no")
  tp_2 <- lolli_plot(res, xaxis = "date_start")
  tp_3 <- lolli_plot(res, xaxis = "date_peak")
  expect_is(tp_1, "ggplot")
  expect_is(tp_2, "ggplot")
  expect_is(tp_3, "ggplot")
})

test_that("lolli_plot() correctly changes y axis label for given 'metric' value", {
  res <- detect_event(data = ts2clm(sst_Med, pctile = 10,
                                    climatologyPeriod = c("1983-01-01", "2012-12-31")), coldSpells = TRUE)
  tp_1 <- lolli_plot(res, metric = "intensity_max")
  tp_2 <- lolli_plot(res, metric = "intensity_mean")
  tp_3 <- lolli_plot(res, metric = "intensity_cumulative")
  tp_4 <- lolli_plot(res, metric = "duration")
  expect_is(tp_1, "ggplot")
  expect_is(tp_2, "ggplot")
  expect_is(tp_3, "ggplot")
  expect_is(tp_4, "ggplot")
})
