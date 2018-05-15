context("Test geoms.R")

# To check specific parts of the plot
# https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
# library(proto)

test_that("geom_flame() doesn't fall over", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$climatology %>%
    dplyr::slice(1300:1500)
  tp <- ggplot(data = res, aes(x = t, y = temp)) +
    geom_flame(aes(y2 = thresh_clim_year))
  expect_is(tp, "ggplot")
})

test_that("geom_lolli() doesn't fall over", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)$event
  tp <- ggplot(data = res) +
    geom_lolli(aes(x = date_start, y = int_cum))
  expect_is(tp, "ggplot")
})
