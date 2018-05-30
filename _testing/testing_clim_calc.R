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

res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))

minDuration <- 5
maxGap <- 2

data <- res_clim
ts_x <- data$t
ts_y <- data$temp
ts_seas <- data$seas
ts_thresh <- data$thresh
t_series <- data.table::data.table(ts_x, ts_y, ts_seas, ts_thresh)

t_series$ts_y[is.na(t_series$ts_y)] <- t_series$ts_seas[is.na(t_series$ts_y)]
t_series$threshCriterion <- as.numeric(t_series$ts_y > t_series$ts_thresh)

# proto_1 <- proto_event(t_series, criterion_column = 5, minDuration = minDuration,
#                        maxGap = maxGap)

criterion_column <- 5

t_series <- as.data.frame(t_series)
ex1 <- rle(as.vector(t_series[, criterion_column])) ###
ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
s1 <- split(zoo::index(t_series[, criterion_column]), ind1)
s1[1:5]

microbenchmark(
  rep(seq_along(ex1$lengths), ex1$lengths),
  rep_vec(seq_along(ex1$lengths), ex1$lengths)
)

s1 <- split(zoo::index(t_series[, criterion_column]), ind1)






# cpp ---------------------------------------------------------------------

rle2_num(t_series$threshCriterion)[1:10,]

# benchmarks --------------------------------------------------------------



profvis(res_climF <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE))
profvis(out <- detect_event(res_climF))


microbenchmark(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE),
               ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE))
