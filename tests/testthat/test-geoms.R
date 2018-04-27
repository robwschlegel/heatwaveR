context("Test geoms.R")

# To check specific parts of the plot
# https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
# library(proto)

test_that("geom_flame() doesn't fall over", {
  res <- detect(data = make_whole(sst_Med),
                climatology_start = 1983, climatology_end = 2012)$clim %>%
    dplyr::slice(1300:1500)
  tp <- ggplot(data = res, aes(x = t, y = temp)) +
    geom_flame(aes(y2 = thresh_clim_year))
  expect_is(tp, "ggplot")
})

test_that("geom_lolli() doesn't fall over", {
  res <- detect(data = make_whole(sst_Med),
                climatology_start = 1983, climatology_end = 2012)$event
  tp <- ggplot(data = res) +
    geom_lolli(aes(x = date_start, y = int_cum))
  expect_is(tp, "ggplot")
})
