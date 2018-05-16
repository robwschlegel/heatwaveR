context("Test graph_functions.R")

# To check specific parts of the plot
# https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
# library(proto)

test_that("event_line() doesn't fall over", {
  res <- detect_event(data = ts2clm(sst_Med,
                climatologyPeriod = c("1983-01-01", "2012-12-31")))
  tp <- event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31")
  expect_is(tp, "ggplot")
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

test_that("lolli_plot() doesn't fall over", {
  res <- detect_event(data = ts2clm(sst_Med,
                climatologyPeriod = c("1983-01-01", "2012-12-31")))
  tp <- lolli_plot(res)
  expect_is(tp, "ggplot")
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

test_that("lolli_plot() correctly changes x axis when xaxis = 'event_no'", {
  res <- detect_event(data = ts2clm(sst_Med, pctile = 10,
                climatologyPeriod = c("1983-01-01", "2012-12-31")), coldSpells = TRUE)
  tp <- lolli_plot(res, xaxis = "event_no")
  expect_is(tp, "ggplot")
})

