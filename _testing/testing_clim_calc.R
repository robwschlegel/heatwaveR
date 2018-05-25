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
maxPadLength <- 1
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

ind <- which(ts_whole$ts_y %in% sample(ts_whole$ts_y, 100))
ts_whole$ts_y[ind] <- NA

# benchmarks --------------------------------------------------------------



profvis(res_climT <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE))
profvis(res_climF <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE))

res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE)
# res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE)
profvis(out <- detect_event(res_clim))


microbenchmark(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE),
               ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE))
