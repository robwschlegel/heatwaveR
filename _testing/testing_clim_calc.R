library(tidyverse)
library(profvis)
library(heatwaveR)
library(microbenchmark)
library(data.table)
library(imputeTS)

climatologyPeriod <- c("1983-01-01", "2012-12-31")
maxPadLength <- 1
windowHalfWidth <- 5
pctile <- 90
smoothPercentile <- TRUE
smoothPercentileWidth <- 31
clmOnly <- TRUE
clim_start <- "1983-01-01"
clim_end <- "2012-12-31"
minDuration <- 5
maxGap <- 2
joinAcrossGaps <- TRUE


res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
data <- res_clim
ts_x <- data$t
ts_y <- data$temp
ts_seas <- data$seas
ts_thresh <- data$thresh
# t_series <- data.frame(ts_x, ts_y, ts_seas, ts_thresh)





# benchmarks --------------------------------------------------------------

library(heatwaveR); library(dplyr); library(ggplot2)
ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
mhw <- detect_event(ts)
mhw$event %>%
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_mean, intensity_max, intensity_cumulative) %>%
  dplyr::arrange(-intensity_cumulative) %>%
  head(5)


res_climF <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE)
profvis(detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE)))
microbenchmark(detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE)))

microbenchmark(
proto_event(t_series, criterion_column = 5,
                       minDuration = minDuration, maxGap = maxGap)
)
