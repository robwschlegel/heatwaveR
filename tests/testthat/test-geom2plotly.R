context("Test geom2plotly.R")

test_that("calling ggplotly() on a figure with geom_flame() in it returns a plotly object", {
  ts_res <- ts2clm(data = sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  ts_res_sub <- ts_res[10300:10800,]
  p <- ggplot(data = ts_res_sub, aes(x = t, y = temp)) +
    geom_flame(aes(y2 = thresh), fill = "salmon") +
    geom_line(aes(y = temp)) +
    geom_line(aes(y = seas), colour = "green") +
    geom_line(aes(y = thresh), colour = "red") +
    labs(x = "", y = "Temperature (°C)")
  pp <- plotly::ggplotly(p)
  expect_is(pp, "plotly")
})

test_that("ggplotly() correctly receives n and n_gap params from geom_flame()", {
  ts_res <- ts2clm(data = sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  ts_res_sub <- ts_res[850:900,]
  p <- ggplot(data = ts_res_sub, aes(x = t, y = temp)) +
    geom_flame(aes(y2 = thresh), n = 5, n_gap = 2) +
    geom_line(aes(y = temp)) +
    geom_line(aes(y = seas), colour = "green") +
    geom_line(aes(y = thresh), colour = "red") +
    labs(x = "", y = "Temperature (°C)")
  pp <- plotly::ggplotly(p)
  expect_is(pp, "plotly")
})
