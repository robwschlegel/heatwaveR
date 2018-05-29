context("Test geoms.R")

# To check specific parts of the plot
# https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
# library(proto)

test_that("geom_flame() doesn't fall over", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$climatology[1300:1500,]
  tp <- ggplot(data = res, aes(x = t, y = temp)) +
    geom_flame(aes(y2 = thresh))
  expect_is(tp, "ggplot")
})

test_that("geom_lolli() doesn't fall over", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$event
  tp <- ggplot(res) +
    geom_lolli(aes(x = date_start, y = intensity_cumulative))
  expect_is(tp, "ggplot")
})

test_that("n = x highlights certain values", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$event
  tp <- ggplot(res, aes(x = date_peak, y = duration)) +
    geom_lolli(n = 3, colour_n = "red") +
    xlab("Peak date") + ylab("Event duration [days]")
  expect_is(tp, "ggplot")
})
