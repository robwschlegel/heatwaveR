context("Test geoms.R")

# To check specific parts of the plot
# https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
# library(proto)

test_that("geom_flame() doesn't fall over", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$climatology[950:1000,]
  tp <- ggplot2::ggplot(data = res, ggplot2::aes(x = t, y = temp)) +
    geom_flame(ggplot2::aes(y2 = thresh))
  expect_is(tp, "ggplot")
})

test_that("geom_flame() screns out heat spikes as desired", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$climatology[800:950,]
  tp <- ggplot2::ggplot(data = res, ggplot2::aes(x = t, y = temp)) +
    geom_flame(ggplot2::aes(y2 = thresh), n = 5, n_gap = 2)
  expect_is(tp, "ggplot")
})

test_that("geom_lolli() doesn't fall over", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$event
  tp <- ggplot2::ggplot(res) +
    geom_lolli(ggplot2::aes(x = date_start, y = intensity_cumulative))
  expect_is(tp, "ggplot")
})

test_that("n = x highlights certain values", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$event
  tp <- ggplot2::ggplot(res, ggplot2::aes(x = date_peak, y = duration)) +
    geom_lolli(n = 3, colour_n = "red") +
    ggplot2::labs(x = "Peak date", y = "Event duration [days]")
  expect_is(tp, "ggplot")
})

test_that("when n = x is greater than the number of data points geom_lolli doesn't fall over", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$event[1:2,]
  tp <- ggplot2::ggplot(res, ggplot2::aes(x = date_peak, y = duration)) +
    geom_lolli(n = 3, colour_n = "red") +
    ggplot2::labs(x = "Peak date", y = "Event duration [days]")
  expect_is(tp, "ggplot")
})

test_that("an empty dataframe doesn't knock geom_lolli over", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$event[0,]
  tp <- ggplot2::ggplot(res, ggplot2::aes(x = date_peak, y = duration)) +
    geom_lolli(n = 3, colour_n = "red") +
    ggplot2::labs(x = "Peak date", y = "Event duration [days]")
  expect_is(tp, "ggplot")
})
