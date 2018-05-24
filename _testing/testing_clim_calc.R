library(tidyverse)
library(profvis)
library(heatwaveR)
library(microbenchmark)
library(data.table)
library(imputeTS)

data <- sst_WA
ts_x <- data$t
ts_y <- data$temp
ts_xy <- data.table(ts_x, ts_y)

climatologyPeriod <- c("1983-01-01", "2012-12-31")
maxPadLength <- 3
windowHalfWidth <- 5
pctile <- 90
smoothPercentile <- TRUE
smoothPercentileWidth <- 31
clmOnly <- TRUE
clim_start <- "1983-01-01"
clim_end <- "2012-12-31"

# ts_xy <- tibble::tibble(ts_x, ts_y)
# rm(ts_x); rm(ts_y)

ts_whole <- make_whole_fast(ts_xy, x = ts_x, y = ts_y)

ts_spread <- clim_spread(ts_whole, clim_start, clim_end, windowHalfWidth)

profvis(res_climT <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE))
profvis(res_climF <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE))

microbenchmark(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE),
               ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE))
