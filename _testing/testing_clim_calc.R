library(data.table)
library(tidyverse)
library(profvis)
library(heatwaveR)
library(microbenchmark)
library(imputeTS)

climatologyPeriod <- c("1983-01-01", "2012-12-31")
maxPadLength <- 1
windowHalfWidth <- 5
pctile <- 90
smoothPercentile <- TRUE
smoothPercentileWidth <- 31
clmOnly <- TRUE
minDuration <- 5
maxGap <- 2
joinAcrossGaps <- TRUE
criterion_column <- "threshCriterion"
minDuration <- 3
gaps <- FALSE

# sst <- fread("/Users/ajsmit/Dropbox/R/Benguela_MUR/sst_test.csv")
data <- sst_WA
# data <- sst
# data <- sst_WA %>%
#   dplyr::mutate(month = month(t)) %>%
#   dplyr::filter(month != 1) %>%
#   dplyr::select(-month)
ts_x <- data$t
ts_y <- data$temp
ts_xy <- data.table::data.table(ts_x, ts_y)

source("R/make_whole_fast.R")
source("R/make_whole.R")
ts_whole <- make_whole_fast(ts_xy)

source("R/na_interp.R")
ts_interped <- na_interp(doy = ts_whole$doy,
                         x = ts_whole$ts_x,
                         y = ts_whole$ts_y,
                         maxPadLength = maxPadLength)

### begin test
# ts_interped %>%
#   dplyr::filter(ts_x >= "2006-01-01" & ts_x <= "2006-01-10")
### end test

clim_start <- climatologyPeriod[1]
clim_end <- climatologyPeriod[2]

source("R/clim_spread.R")

ts_wide <- clim_spread(ts_interped, clim_start, clim_end, windowHalfWidth)

source("src/clim_calc.cpp") # does not work with NAs
# source("R/clim_calc.R") # works
ts_mat <- heatwaveR:::clim_calc_cpp(ts_wide, windowHalfWidth, pctile)

source("R/smooth_percentile.R")
ts_clim <- smooth_percentile(ts_mat, smoothPercentileWidth, var = FALSE)

# benchmarks --------------------------------------------------------------

profvis(detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE)))
microbenchmark(detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE)))

microbenchmark(
proto_event(t_series, criterion_column = 5,
                       minDuration = minDuration, maxGap = maxGap)
)


ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE)
