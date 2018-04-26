context("Test graph_functions.R")

# To check specific parts of the plot
# https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
# library(proto)

test_that("event_line() doesn't fall over", {
  res <- detect(data = make_whole(sst_Med),
                climatology_start = 1983, climatology_end = 2012)
    dplyr::slice(1300:1500)
  tp <- event_line(data = res, start_date = "2012-01-01", end_date = "2012-12-31")
  expect_is(tp, "ggplot")
})

test_that("lolli_plot() doesn't fall over", {
  res <- detect(data = make_whole(sst_Med),
                climatology_start = 1983, climatology_end = 2012)
  tp <- lolli_plot(res)
  expect_is(tp, "ggplot")
})
